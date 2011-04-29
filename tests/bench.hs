-- A little benchmark adapted from attoparsec

module Main where

import Control.Monad
-- import qualified Data.ByteString.Lazy as L
import System.Environment

import Data.IterIO
import Data.IterIO.Parse

iterio :: IO ()
iterio = do
  args <- getArgs
  forM_ args $ \arg -> do
           result <- enumFile' arg |$ p
           print (length result)
 where
  fast = many (while1I isLetter <|> while1I isDigit)
  isDigit c  = c >= eord '0' && c <= eord '9'
  isLetter c = (c >= eord 'a' && c <= eord 'z')
               || (c >= eord 'A' && c <= eord 'Z')
  p = fast

main :: IO ()
main = iterio
