
module Main where

import Data.IterIO.Base
import Data.IterIO.Extra

fmtest :: IO ()
fmtest = run $ feedI testiter (chunk ())
    where 
      testiter = IterF iterf
      iterf (Chunk _ eof) = iterm >> if eof then return () else testiter
      iterm = IterM (return $ return ())

main :: IO ()
main = fmtest
