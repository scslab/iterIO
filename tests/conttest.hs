
module Main where

import Control.Monad.Cont
import Control.Monad.Trans
import Data.IterIO.Base
import Data.IterIO
import Debug.Trace

conttest :: IO ()
conttest = enumPure "foo" `cat` enumPure "bar" |$ iter
    where   
      iter :: Iter String IO ()
      iter = runContTI $ do
               callCC $ \cc -> do
                           callCC $ ($ ())
                           liftIO $ return ()
                           cc ()
                           liftIO $ putStrLn "does not execute"
               foo <- pureI
               if foo == "foobar" then return ()
                 else error $ "failure: foobar /= " ++ foo

xtrace :: (ChunkData t, Monad m) => String -> Iter t m ()
xtrace tag = do
  input <- pureI
  trace (tag ++ ": " ++ chunkShow input) $ Done () (chunk input)

conttest2 :: IO String
conttest2 = enumPure "test string\n" |$ iter
    where
      iter :: Iter String IO String
      iter = do
        xtrace "0"
        ungetI "prepend: "
        xtrace "0'"
        runContTI $ do
               callCC $ \cc -> do
                      xtrace "1"
                      xtrace "1'"
                      ungetI "prepend: "
                      xtrace "2"
                      ungetI "lost: "
                      liftIO $ putStrLn "hello world"
                      cc ()
                      liftIO $ putStrLn "goodbye world"
                      xtrace "3"
                      stdoutI
        xtrace "4"
        pureI

testeof :: IO ()
testeof = run iter
    where
      iter :: Iter String IO ()
      iter = IterF $ \_ -> return () >> return ()

data Bind a = Bind String a deriving (Show)

instance Monad Bind where
    return a = Bind "return" a
    (Bind s1 a1) >>= k =
        case k a1 of
          Bind s2 a2 -> Bind ("(" ++ s1 ++ " >>= " ++ s2 ++ ")") a2

foo :: Bind Int
foo = do
  x <- Bind "x" 7
  y <- Bind "y" 8
  z <- Bind "z" 9
  return $ x + y + z

main :: IO ()
main = conttest
