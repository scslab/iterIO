
module Main where

import Control.Monad.Cont
import Control.Monad.Trans
import Data.IterIO

conttest :: Iter String IO ()
conttest = runContTI $ do
  callCC $ \cc -> do
    callCC $ ($ ())
    liftIO $ return ()
    cc ()
    liftIO $ putStrLn "does not execute"
  foo <- pureI
  if foo == "foobar" then return () else error $ "failure: foobar /= " ++ foo

main :: IO ()
main = enumPure "foo" `cat` enumPure "bar" |$ conttest
