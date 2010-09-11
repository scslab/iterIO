
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import Data.IterIO.Base
import Data.IterIO.ListLike
import Data.IterIO.Inum
import Data.IterIO.Extra

minusOne :: Iter [Int] IO () -> Iter [Int] IO ()
minusOne iout = do
  x <- headLI
  iout' <- inumPure [x - 1] iout
  if x <= 0 then runI iout' else minusOne iout'

inumPrintList :: Inum [Int] [Int] IO a
inumPrintList = mkInum' $ do
                  x <- dataI
                  liftIO $ S8.putStrLn $ S8.pack (show x)
                  return x

pong :: IO ()
pong = do
  sem <- newQSemN 0
  (iterA, enumB) <- iterLoop
  (iterB, enumA) <- iterLoop
  _ <- forkIO $ do
         -- tidTrace "thread A"
         handle (\e@(SomeException _) -> tidTrace $ show e) $
                (inumPure [10] `cat` enumA |. inumNop) |. inumNop
                    |$ inumNop .| inumPrintList .| minusOne iterA
         -- tidTrace "thread A done"
         signalQSemN sem 1
  _ <- forkIO $ do
         -- tidTrace "thread B"
         handle (\e@(SomeException _) -> tidTrace $ show e) $
                enumB |. inumNop |$ inumNop .| iterB >> signalQSemN sem 1
         -- tidTrace "thread B done"
         signalQSemN sem 1
  waitQSemN sem 2
  S8.putStrLn $ S8.pack "Done"

main :: IO ()
main = pong
