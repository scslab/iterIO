
module NetSim where

-- import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.Trans
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Word
import Network.Socket
import System.Time

import TM
import Arc4
import Data.IterIO
import Data.IterIO.Extra

type NetSim a = Inum [L.ByteString] [L.ByteString] TM a
type NetSimM = Iter [L.ByteString] TM

udpI :: (MonadIO m) => Socket -> Iter [L.ByteString] m ()
udpI s = do
  _ <- error "udpI"
  packet <- headI
  liftIO $ genSend s packet
  udpI s

rndBoolI :: Float -> NetSimM Bool
rndBoolI prob = lift $ asks tcRnd >>= flip a4RandomBool prob

rndIntI :: (Integral a, Integral b, Bits b) => a -> NetSimM b
rndIntI bound = do
  r <- lift $ asks tcRnd >>= a4RandomBits
  return $ abs r `mod` (fromIntegral bound)


dropper :: Float -> NetSim a
dropper dropProb = mkInum loop
    where loop = do
            packet <- headI
            dropit <- rndBoolI dropProb
            if dropit then loop else return [packet]

reorderer :: Float -> NetSim a
reorderer prob = mkInumAutoM $ headLI >>= oldOrNew
  where
    oldOrNew old = do
      new <- headLI
      b <- liftI $ rndBoolI prob
      if b then ifeed [old] >> oldOrNew new
           else ifeed [new] >> oldOrNew old

duplicater :: Float -> NetSim a
duplicater dupProb = mkInum $ do
  packet <- headI
  dupit <- rndBoolI dupProb
  return $ (packet:if dupit then [packet] else [])

badlength :: Float -> NetSim a
badlength prob = mkInum $ do
  pkt <- headI
  doit <- rndBoolI prob
  return [if doit then corrupt pkt else pkt]
    where
      corrupt pkt | L.null pkt = pkt
                  | otherwise  = L.cons (L.head pkt .|. 0x80)
                                 (L.drop 1 pkt)

garbage :: Float -> NetSim a
garbage prob = mkInum $ do
  pkt <- headI
  doit <- rndBoolI prob
  if doit
    then do rlen <- rndIntI (513 :: Int)
            crap <- lift $ asks tcRnd >>= flip a4RandomStringN rlen
            return [crap, pkt]
    else return [pkt]

corrupter :: Float -> NetSim a
corrupter prob = mkInum $ do
  pkt <- headI
  doit <- rndBoolI prob
  pkt' <- (if doit then corrupt else return) pkt
  return [pkt']
    where
      corrupt pkt | L.null pkt = return pkt
                  | otherwise = do
                        rbit <- liftM ((1::Word8) `shiftL`) $ rndIntI (8 :: Int)
                        rbyte <- rndIntI $ L.length pkt
                        let (a, b') = L.splitAt rbyte pkt
                            (b, c) = L.splitAt 1 b'
                            rb = L.index b 0 `xor` rbit
                        return $ L.append a $ L.cons rb c
        
truncater :: Float -> NetSim a
truncater prob = mkInum $ do
  pkt <- headI
  doit <- rndBoolI prob
  return [if doit then L.init pkt else pkt]

subtime :: ClockTime -> ClockTime -> ClockTime
subtime (TOD as aps) (TOD bs bps) =
    let s = as - bs
        p = aps - bps
    in if p >= 0 then TOD s p else TOD (s - 1) (p + 1000000000)

leqtime :: ClockTime -> ClockTime -> Bool
leqtime (TOD as aps) (TOD bs bps) =
    as < bs || as == bs && aps <= bps

excessive :: NetSim a
excessive = mkInumM $ doit [] Nothing
    where
      -- holdtime :: InumM [L.ByteString] [L.ByteString] TM a ClockTime
      holdtime = do to <- lift $ asks tcTimeout
                    let ms = to `div` 2
                    return $ TOD (fromIntegral $ ms `div` 1000)
                                 (fromIntegral $ (ms `mod` 1000 * 1000000))
      -- doit :: [L.ByteString] -> (Maybe ClockTime)
      --      -> InumM [L.ByteString] [L.ByteString] TM a ()
      doit pkts' mtime = do
        pkt <- headLI
        let pkts = pkt:pkts'
        now <- liftIO getClockTime
        time <- case mtime of
                  Just time' -> return time'
                  Nothing -> liftIO getClockTime
        ht <- holdtime
        if (now `subtime` time) `leqtime` ht
          then doit pkts (Just time)
          else if length pkts > 6
               then nullI  -- Kill connection
               else do ifeed (reverse pkts) >> ipipe inumNop >> return ()
