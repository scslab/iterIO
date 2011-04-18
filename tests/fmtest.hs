
module Main where

import Control.Exception
import Data.Monoid

import Control.Monad.Trans
import Data.IterIO.Iter
import Data.IterIO.Extra
import Data.IterIO.Parse

{-
-- | This shouldn't mappend to EOF, but did when there was a bug in feedI
eof1 :: Iter () IO ()
eof1 = feedI (IterF $ IterFail (toException $ ErrorCall "error"))
       (Chunk mempty True)
-}

import Debug.Trace

-- | These shouldn't mappend to EOF, but did when there was a bug in feedI

eoftst1 :: IO ()
eoftst1 = run $ ifParse dataI return $ return ()

eoftst2 :: IO ()
eoftst2 = run $ catchBI dataI $ \(SomeException _) -> return ()

{-
eoftst3 :: IO ()
eoftst3 = run (copyInput dataI >>= check :: Iter () IO ())
    where check (iter, input) = feedI (return ()) input
-}

eoftst4 :: IO ()
eoftst4 = enumPure () |$ multiParse dataI (IterF $ \(Chunk t _) -> return t)

fmtest :: IO ()
fmtest = enumPure () |$ testiter
    where
      testiter = iterF iterf -- iterF prevents infinite loop IterF wouldn't
      iterf (Chunk _ eof) = iterm >> if eof then return () else testiter
      iterm = IterM (return $ return ())

hangtest :: IO ()
hangtest = enumPure () |$ iter
    where
      iter = iterF $ \(Chunk _ eof) -> do
               lift $ return ()
               if eof then return () else iter

main :: IO ()
main = do
  fmtest
  eoftst1
  eoftst2
  eoftst4
  return ()
