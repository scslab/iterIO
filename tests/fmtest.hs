
module Main where

import Control.Exception
import Data.Monoid

import Data.IterIO.Base
import Data.IterIO.Extra

-- | This shouldn't mappend to EOF, but did when there was a bug in feedI
mappendEOF :: Iter () IO ()
mappendEOF = feedI (IterF $ IterFail (toException $ ErrorCall "error"))
             (Chunk mempty True)

fmtest :: IO ()
fmtest = run $ feedI testiter (chunk ())
    where
      testiter = iterF iterf -- iterF prevents infinite loop IterF wouldn't
      iterf (Chunk _ eof) = iterm >> if eof then return () else testiter
      iterm = IterM (return $ return ())

main :: IO ()
main = fmtest
