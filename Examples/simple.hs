
module Main
    ( module Main
    , module Data.IterIO
    ) where

import Control.Monad.Trans
import System.FilePath
import System.IO

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

liftIOexample :: Iter String IO ()
liftIOexample = do
  line <- lineI
  liftIO $ putStrLn $ "First line is: " ++ line
  next <- stringExactI 40
  liftIO $ putStrLn $ "And the next 40 bytes are: " ++ next

main :: IO ()
main = return ()
