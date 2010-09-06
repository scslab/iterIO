
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


inumGrep' :: (MonadIO m) => String -> Inum L.ByteString L.ByteString m a
inumGrep' re iter = do
  Right cre <- liftIO $ compile 0 0 $ S8.pack re
  flip enumI' iter $ do
    line <- lineI
    Right amatch <- liftIO $ execute cre (S.concat $ L.toChunks line)
    return $ if isJust amatch
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
      handler :: IOError -> OnumR [S.ByteString] IO a
              -> OnumR [S.ByteString] IO a
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

inumToLines :: (Monad m) => Inum S.ByteString [S.ByteString] m a
inumToLines = enumI' $ do
                line <- lineI
                return [line]

inumGrep :: (Monad m) => String -> Inum [S.ByteString] [S.ByteString] m a
inumGrep re = enumI' $ do
  line <- headI
  return $ if line =~ packedRe then [line] else []
    where
      packedRe = S8.pack re

lengthI :: (Show t, Monad m) => Iter [t] m Int
lengthI = count 0
    where
      count n = do
        line <- safeHeadI
        case line of
          Just _  -> count (n+1)
          Nothing -> return n

catchTest1 :: IO ()
catchTest1 = myEnum |$ fail "bad Iter"
    where
      myEnum :: Onum String IO ()
      myEnum iter = catchI (enumPure "test" iter) handler
      handler (SomeException _) _ = do
        liftIO $ hPutStrLn stderr "ignoring exception"
        return $ return ()


inumBad :: (ChunkData t, Monad m) => Inum t t m a
inumBad = enumI $ fail "inumBad"

catchTest2 :: IO ()
catchTest2 = myEnum |.. inumBad |$ nullI
    where
      myEnum :: Onum String IO (Iter String IO ())
      myEnum iter = catchI (enumPure "test" iter) handler
      handler (SomeException _) _ = do
        liftIO $ hPutStrLn stderr "ignoring exception"
        return $ return $ return ()

skipError :: (ChunkData t, MonadIO m) =>
             SomeException -> OnumR t m a -> OnumR t m a
skipError e iter = do
  liftIO $ hPutStrLn stderr $ "skipping error: " ++ show e
  resumeI iter

-- Throws an exception
test1 :: IO ()
test1 = enumCatch (enumPure "test") skipError |.. inumBad |$ nullI

-- Does not throw an exception, because inumCatch catches
-- all errors, including from subsequently fused inumBad.
test2 :: IO ()
test2 = inumCatch (enumPure "test") skipError |.. inumBad |$ nullI

-- Does not throw an exception, because inumBad fused within the
-- argument to enumCatch.
test3 :: IO ()
test3 = enumCatch (enumPure "test" |.. inumBad) skipError |$ nullI

main :: IO ()
main = do
  n <- grepCount
  putStrLn $ show n
