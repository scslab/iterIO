
module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Exit
import System.IO

import Data.IterIO

filterLines :: (Monad m) =>
               String
            -> EnumI L.ByteString [L.ByteString] m a
filterLines s = enumI' $ do
                  line <- lineI
                  return $ if match line then [line] else []
    where
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

enumFileCatchError :: (MonadIO m) => FilePath -> EnumO L.ByteString m a
enumFileCatchError file = handlerI printErrorAndResume . enumFile file
    where
      printErrorAndResume (SomeException _) iter = verboseResumeI iter

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
  enum |.. filterLines pat |$ printLines
  exitSuccess
