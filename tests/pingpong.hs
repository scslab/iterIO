
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import Data.IterIO.Base
import Data.IterIO.ListLike
import Data.IterIO.Inum
import Data.IterIO.Extra

minusOne :: Int -> Iter [Int] IO () -> Iter [Int] IO ()
minusOne expect iout = do
  x <- headLI
  when (x /= expect) $ error $ "expected " ++ show expect ++ ", got " ++ show x
  iout' <- enumPure [x - 1] iout
  if x <= 0 then runI iout' else minusOne (x - 1) iout'

inumPrintList :: Inum [Int] [Int] IO a
inumPrintList = mkInum' $ do
                  x <- dataI
                  liftIO $ S8.putStrLn $ S8.pack (show x)
                  return x

ping :: IO ()
ping = do
  (iterA, enumA) <- iterLoop
  (enumPure [10] `cat` enumA |. inumNop) |. inumNop
       |$ inumNop .| inumPrintList .| minusOne 10 iterA

pong :: IO ()
pong = do
  sem <- newQSemN 0
  (iterA, enumB) <- iterLoop
  (iterB, enumA) <- iterLoop
  _ <- forkIO $ do
         (enumPure [10] `cat` enumA |. inumNop) |. inumNop
                |$ inumNop .| inumPrintList .| minusOne 10 iterA
         signalQSemN sem 1
  _ <- forkIO $ do
         enumB |. inumNop |$ inumNop .| iterB >> signalQSemN sem 1
         signalQSemN sem 1
  waitQSemN sem 2
  S8.putStrLn $ S8.pack "Done"

main :: IO ()
main = ping >> pong
