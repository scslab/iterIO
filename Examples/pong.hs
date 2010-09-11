
module Main where

import Control.Concurrent
import Control.Monad.Trans
import Data.ByteString.Char8 as S8
import Data.IterIO
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

iterLoop2 :: QSemN -> IO (Iter [Int] IO (), Onum [Int] IO a)
iterLoop2 sem = do
  (iterA, enumB) <- iterLoop
  (iterB, enumA) <- iterLoop
  forkIO $ enumB |. inumNop |$ inumNop .| iterB >> signalQSemN sem 1
  return (iterA, enumA)

pong :: IO ()
pong = do
  sem <- newQSemN 0
  (iter, enum) <- iterLoop2 sem
  inumPure [10] `cat` enum |$ inumPrintList .| minusOne iter
  waitQSemN sem 1

main :: IO ()
main = pong
