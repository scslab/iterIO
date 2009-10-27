
module Main
    ( module Main
    , module Data.IterIO
    ) where

import Control.Exception
import Control.Monad.Trans
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
-- import System.FilePath
import System.IO
import System.IO.Error
import Text.Regex.Posix
import Text.Regex.Posix.ByteString

import Data.IterIO

-- | Copy file to standard output
catFile :: FilePath -> IO ()
catFile path = enumFile' path |$ handleI stdout

-- | Return first line of file
headFile :: FilePath -> IO String
headFile path = enumFile path |$ lineI

-- | Return first two lines of file
head2File :: FilePath -> IO (String, String)
head2File path = enumFile path |$ lines2I

lines2I :: (Monad m) => Iter String m (String, String)
lines2I = do
  line1 <- lineI
  line2 <- lineI
  return (line1, line2)

liftIOexampleI :: (MonadIO m) => Iter String m ()
liftIOexampleI = do
  line <- lineI
  liftIO $ putStrLn $ "First line is: " ++ line
  next <- stringExactI 40
  liftIO $ putStrLn $ "And the next 40 bytes are: " ++ next

lineCountI :: (Monad m) => Iter String m Int
lineCountI = count 0
    where
      count n = do
        line <- safeLineI
        case line of
          Just _  -> count (n+1)
          Nothing -> return n


inumGrep' :: (MonadIO m) => String -> EnumI L.ByteString L.ByteString m a
inumGrep' re iter = do
  Right cre <- liftIO $ compile 0 0 $ S8.pack re
  flip enumI' iter $ do
    line <- lineI
    Right match <- liftIO $ execute cre (S.concat $ L.toChunks line)
    return $ if isJust match
             then L8.snoc line '\n'
             else mempty

grepCount :: IO Int
grepCount = enumFile "/usr/share/dict/words" |.. inumToLines
                `cat` enumFile "/usr/share/dict/extra.words" |.. inumToLines
            |$ inumGrep "kk"
                    ..| inumGrep "^[a-z]"
                    ..| lengthI

grep :: String -> [FilePath] -> IO ()
grep re files
    | null files = enumHandle stdin |.. inumToLines |$ inumGrep re ..| linesOutI
    | otherwise  = foldr1 cat (map enumLines files) |$ inumGrep re ..| linesOutI
    where
      enumLines file = enumCatch (enumFile file |.. inumToLines) handler
      handler :: IOError -> Iter [S.ByteString] IO a -> Iter [S.ByteString] IO a
      handler e iter = do
        liftIO (hPutStrLn stderr $ show e)
        if isDoesNotExistError e
          then resumeI iter
          else iter
      linesOutI = do
        mline <- safeHeadI
        case mline of
          Just line -> do liftIO $ S.putStrLn line
                          linesOutI
          Nothing -> return ()

inumToLines :: (Monad m) => EnumI S.ByteString [S.ByteString] m a
inumToLines = enumI' $ do
                line <- lineI
                return [line]

inumGrep :: (Monad m) => String -> EnumI [S.ByteString] [S.ByteString] m a
inumGrep re = enumI' $ do
  line <- headI
  return $ if line =~ packedRe then [line] else []
    where
      packedRe = S8.pack re

lengthI :: (Monad m) => Iter [t] m Int
lengthI = count 0
    where
      count n = do
        line <- safeHeadI
        case line of
          Just _  -> count (n+1)
          Nothing -> return n

inumBad :: (ChunkData t, Monad m) => EnumI t t m a
inumBad = enumI $ fail "inumBad"

skipError :: (ChunkData t, MonadIO m) =>
               SomeException -> Iter t m a -> Iter t m a
skipError e iter = do
  liftIO $ hPutStrLn stderr $ "skipping error: " ++ show e
  resumeI iter

-- Throws an exception
test1 :: IO ()
test1 = enumCatch (enumPure "test") skipError |.. inumBad |$ nullI

-- Does not throw an exception, because enumCatch' catches
-- all errors, including from subsequently fused inumBad.
test2 :: IO ()
test2 = enumCatch' (enumPure "test") skipError |.. inumBad |$ nullI

-- Does not throw an exception, because inumBad fused within the
-- argument to enumCatch.
test3 :: IO ()
test3 = enumCatch (enumPure "test" |.. inumBad) skipError |$ nullI

main :: IO ()
main = do
  n <- grepCount
  putStrLn $ show n
