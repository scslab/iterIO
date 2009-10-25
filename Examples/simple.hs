
module Main
    ( module Main
    , module Data.IterIO
    ) where

import System.FilePath
import System.IO

import Data.IterIO

-- Copy file to standard output
catFile :: FilePath -> IO ()
catFile path = enumFile path |$ (handleI stdout :: Iter String IO ())

-- Return first line of file
headFile :: FilePath -> IO String
headFile path = enumFile path |$ lineI

main :: IO ()
main = return ()
