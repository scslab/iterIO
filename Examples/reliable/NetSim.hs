
module NetSim where

-- import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Word
import Network.Socket
import System.Time

import TM
import Arc4
import Data.IterIO
import Data.IterIO.Extra

type NetSim a = EnumI [L.ByteString] [L.ByteString] TM a
type NetSimM = Iter [L.ByteString] TM

udpI :: (MonadIO m) => Socket -> Iter [L.ByteString] m ()
udpI s = do
  error "udpI"
  packet <- headI
  liftIO $ sendStr s packet
  udpI s

rndBoolI :: Float -> NetSimM Bool
rndBoolI prob = lift $ asks tcRnd >>= flip a4RandomBool prob

rndIntI :: (Integral a, Integral b, Bits b) => a -> NetSimM b
rndIntI bound = do
  r <- lift $ asks tcRnd >>= a4RandomBits
  return $ abs r `mod` (fromIntegral bound)


dropper :: Float -> NetSim a
dropper dropProb iter = do
  packet <- headI
  dropit <- rndBoolI dropProb
  if dropit
    then dropper dropProb iter
    else feedI (dropper dropProb) [packet] iter

reorderer :: Float -> NetSim a
reorderer prob iter1 = do
  packet <- headI
  oldOrNew packet iter1
    where
      oldOrNew old iter =
          do new <- headI
             b <- rndBoolI prob
             feedI (oldOrNew $ if b then new else old)
                   [if b then old else new] iter

duplicater :: Float -> NetSim a
duplicater dupProb iter = do
  packet <- headI
  dupit <- rndBoolI dupProb
  feedI (duplicater dupProb) (packet:if dupit then [packet] else []) iter

badlength :: Float -> NetSim a
badlength prob iter = do
  pkt <- headI
  doit <- rndBoolI prob
  pkt' <- (if doit then corrupt else return) pkt
  feedI (corrupter prob) [pkt'] iter
    where
      corrupt pkt | L.null pkt = return pkt
                  | otherwise  = return $ L.cons (L.head pkt .|. 0x80)
                                 (L.drop 1 pkt)

garbage :: Float -> NetSim a
garbage prob iter = do
  pkt <- headI
  doit <- rndBoolI prob
  if doit
    then do rlen <- rndIntI 513
            crap <- lift $ asks tcRnd >>= flip a4RandomStringN rlen
            feedI (garbage prob) [crap,pkt] iter
    else feedI (garbage prob) [pkt] iter

corrupter :: Float -> NetSim a
corrupter prob iter = do
  pkt <- headI
  doit <- rndBoolI prob
  pkt' <- (if doit then corrupt else return) pkt
  feedI (corrupter prob) [pkt'] iter
    where
      corrupt pkt | L.null pkt = return pkt
                  | otherwise = do
                        rbit <- liftM ((1::Word8) `shiftL`) $ rndIntI 8
                        rbyte <- rndIntI $ L.length pkt
                        let (a, b') = L.splitAt rbyte pkt
                            (b, c) = L.splitAt 1 b'
                            rb = L.index b 0 `xor` rbit
                        return $ L.append a $ L.cons rb c
        
truncater :: Float -> NetSim a
truncater prob iter = do
  pkt <- headI
  doit <- rndBoolI prob
  feedI (truncater prob) [if doit then L.init pkt else pkt] iter

subtime :: ClockTime -> ClockTime -> ClockTime
subtime (TOD as aps) (TOD bs bps) =
    let s = as - bs
        p = aps - bps
    in if p >= 0 then TOD s p else TOD (s - 1) (p + 1000000000)

leqtime :: ClockTime -> ClockTime -> Bool
leqtime (TOD as aps) (TOD bs bps) =
    as < bs || as == bs && aps <= bps

excessive :: NetSim a
excessive = doit [] Nothing
    where
      holdtime = do to <- lift $ asks tcTimeout
                    let ms = to `div` 2
                    return $ TOD (fromIntegral $ ms `div` 1000)
                               (fromIntegral $ (ms `mod` 1000 * 1000000))
      dead = IterF $ \_ -> return $ dead
      doit pkts' mtime iter = do
        pkt <- headI
        let pkts = pkt:pkts'
        now <- liftIO getClockTime
        time <- case mtime of
                  Just time' -> return time'
                  Nothing -> liftIO getClockTime
        ht <- holdtime
        if (now `subtime` time) `leqtime` ht
          then doit pkts (Just time) iter
          else if length pkts > 6
               then dead
               else feedI inumNop (reverse pkts) iter
