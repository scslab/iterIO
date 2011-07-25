
module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L

import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Exit
import System.IO
-- import System.IO.Error

import Data.IterIO

filterLines :: (Monad m) =>
               String
            -> Inum L.ByteString [L.ByteString] m a
filterLines s = mkInum loop
    where
      loop = do line <- lineI
                if match line then return [line] else return []
      ls = L8.pack s
      match l | L.null l  = False
              | otherwise = L.isPrefixOf ls l || match (L.tail l)

printLines :: (MonadIO m) => Iter [L.ByteString] m ()
printLines = do
  line <- safeHeadI
  case line of
    Just l -> do liftIO $ L.putStrLn l
                 printLines
    Nothing -> return ()

enumFileCatchError :: (MonadIO m) => FilePath -> Onum L.ByteString m a
enumFileCatchError file = enumFile file `inumCatch` enumCatchIO
    where
      enumCatchIO :: (ChunkData t, MonadIO m) =>
                     IOError
                  -> IterR () m (IterR t m a)
                  -> Iter () m (IterR t m a)
      enumCatchIO _ = verboseResumeI
      -- or to avoid the need for a type signature, you could say:
      -- enumCatchIO e = flip const (e :: IOError) verboseResumeI

main :: IO ()
main = do
  prog <- getProgName
  av <- getArgs
  unless (length av >= 1) $ do
         hPutStrLn stderr $ "usage: " ++ prog ++ " string [file ...]"
         exitFailure
  hSetBuffering stdout NoBuffering
  let pat = head av
      enum = if length av == 1
             then enumHandle stdin
             else foldr1 cat $ map enumFileCatchError $ tail av
  enum |. filterLines pat |$ printLines
  exitSuccess
