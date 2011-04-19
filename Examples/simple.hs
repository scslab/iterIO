
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
  next <- takeExactI 40
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
  flip mkInumP iter $ do
    line <- lineI
    Right amatch <- liftIO $ execute cre (S.concat $ L.toChunks line)
    return $ if isJust amatch
             then L8.snoc line '\n'
             else mempty

grepCount :: IO Int
grepCount = enumFile "/usr/share/dict/words" |. inumToLines
                `cat` enumFile "/usr/share/dict/extra.words" |. inumToLines
            |$ inumGrep "kk"
                    .| inumGrep "^[a-z]"
                    .| lengthI

grep :: String -> [FilePath] -> IO ()
grep re files
    | null files = enumHandle stdin |. inumToLines |$ inumGrep re .| linesOutI
    | otherwise  = foldr1 cat (map enumLines files) |$ inumGrep re .| linesOutI
    where
      enumLines file = inumCatch (enumFile file |. inumToLines) handler
--      handler :: IOError -> IterR () IO (IterR [S.ByteString] IO a)
--              -> Iter () IO (IterR [S.ByteString] IO a)
      handler :: IOError -> IterR () IO (IterR [S.ByteString] IO a)
              -> Iter () IO (IterR [S.ByteString] IO a)
      handler e iter = do
        liftIO (hPutStrLn stderr $ show e)
        if isDoesNotExistError e
          then resumeI iter
          else reRunIter iter
      linesOutI = do
        mline <- safeHeadI
        case mline of
          Just line -> do liftIO $ S.putStrLn line
                          linesOutI
          Nothing -> return ()

inumToLines :: (Monad m) => Inum S.ByteString [S.ByteString] m a
inumToLines = mkInum $ do
                line <- lineI
                return [line]

inumGrep :: (Monad m) => String -> Inum [S.ByteString] [S.ByteString] m a
inumGrep re = mkInumP $ do
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

inumBad :: (ChunkData t, Monad m) => Inum t t m a
inumBad = mkInum $ fail "inumBad"

catchTest1 :: IO ()
catchTest1 = myEnum |$ fail "bad Iter"
    where
      myEnum :: Onum String IO ()
      myEnum iter = catchI (enumPure "test" .| iter) handler
                    >>= return . flip Done mempty
      handler (SomeException _) _ = do
        liftIO $ hPutStrLn stderr "ignoring exception"
        return ()

-- Doesn't work, because the failure is not iter but inside iter
catchTest2 :: IO ()
catchTest2 = myEnum |. inumNop |$ fail "bad Iter"
    where
      myEnum :: Onum String IO (IterR String IO ())
      myEnum iter = catchI (enumPure "test" .| iter) handler
                    >>= return . flip Done mempty
      handler (SomeException _) _ = do
        liftIO $ hPutStrLn stderr "ignoring exception"
        return $ Done () mempty

skipError :: (ChunkData tOut, MonadIO m) =>
             SomeException
          -> IterR tOut m (IterR tIn m a)
          -> Iter tOut m (IterR tIn m a)
skipError e iter = do
  liftIO $ hPutStrLn stderr $ "skipping error: " ++ show e
  resumeI iter

resumeTest :: IO ()
resumeTest = doFile "file1" `cat` doFile "file2" |$ handleI stdout
    where
      doFile path = inumCatch (enumFile' path) $ \err iter ->
                    if isDoesNotExistError err
                    then verboseResumeI iter
                    else reRunIter iter

-- Throws an exception, because inumBad was fused outside the argument
-- to inumCatch.
test1 :: IO ()
test1 = inumCatch (enumPure "test") skipError |. inumBad |$ nullI

-- Does not throw an exception, because inumBad fused within the
-- argument to enumCatch.
test2 :: IO ()
test2 = inumCatch (enumPure "test" |. inumBad) skipError |$ nullI

-- Again no exception, because inumCatch is wrapped around inumBad.
test3 :: IO ()
test3 = enumPure "test" |. (inumCatch inumBad skipError) |$ nullI

-- Catches exception, because .|$ propagates failure through the outer
-- Iter Monad, where it can still be caught.
apply1 :: IO String
apply1 = enumPure "test1" |$ iter `catchI` handler
    where
      iter = enumPure "test2" .|$ fail "error"
      handler (SomeException _) _ = return "caught error"

-- Does not catch error, because |$ turns an Iter failure into a
-- language-level exception, which can only be caught in IO Monad.
apply2 :: IO String
apply2 = enumPure "test1" |$ iter `catchI` handler
    where
      iter = lift (enumPure "test2" |$ fail "error")
      handler (SomeException _) _ = return "caught error"

-- Catches the exception, because liftIO uses the IO catch function to
-- turn language-level exceptions into Monadic Iter failures.  (By
-- contrast, lift must work for all Monads so cannot this in apply2.)
apply3 :: IO String
apply3 = enumPure "test1" |$ iter `catchI` handler
    where
      iter = liftIO (enumPure "test2" |$ fail "error")
      handler (SomeException _) _ = return "caught error"



main :: IO ()
main = do
  n <- grepCount
  putStrLn $ show n
