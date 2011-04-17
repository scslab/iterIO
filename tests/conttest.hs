
module Main where

import Control.Monad.Cont
import Control.Monad.Trans
import Data.IterIO.Iter
import Data.IterIO
import Debug.Trace

conttest :: IO ()
conttest = inumPure "123foo" `cat` inumPure "bar" |$ iter
    where   
      iter :: Iter String IO ()
      iter = runContTI $ do
               callCC $ \cc -> do
                           _ <- headLI
                           callCC $ ($ ())
                           _ <- headLI
                           liftIO $ return ()
                           cc ()
                           liftIO $ putStrLn "does not execute"
               foo <- pureI
               if foo == "3foobar" then return ()
                 else error $ "failure: 3foobar /= " ++ foo

xtrace :: (ChunkData t, Monad m) => String -> Iter t m ()
xtrace tag = Iter $ \c@(Chunk input _) ->
             trace (tag ++ ": " ++ chunkShow input) $ Done () c

conttest2 :: IO String
conttest2 = inumPure "test string\n" |$ iter
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
      iter = Iter $ \_ -> IterF $ return () >> return ()

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
