{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Protocol where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
-- import Data.Int
-- import Data.List
import Data.Maybe
-- import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
-- import Foreign.Ptr
import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Internal as L
-- import qualified Data.ByteString.Lazy.Char8 as L8
import Network.Socket
-- import System.IO

import Data.IterIO

portOfAddr :: SockAddr -> PortNumber
portOfAddr (SockAddrInet port _)      = port
portOfAddr (SockAddrInet6 port _ _ _) = port
portOfAddr _                          = 0

type SeqNo = Word32
data Packet = AckP { ackNo :: SeqNo }
            | DataP { acknoP :: SeqNo
                    , seqnoP :: SeqNo
                    , payloadP :: L.ByteString
                    } deriving (Show)
type Queue = Map SeqNo Packet

data Endpoint = Endpoint
    { epWin :: SeqNo        -- ^ Window size
    , epTimeout :: Int      -- ^ Retransmission timeout
    , epAck :: SampleVar () -- ^ Receiver pokes sender when acks received
    , epSnd :: SampleVar () -- ^ Sender pokes recever when sent data
    , epLFR :: MVar SeqNo -- ^ Largest frame received (to put in acks)
    , epLAR :: MVar SeqNo -- ^ Largest acknowledgment received
    , epLFS :: MVar SeqNo -- ^ Largest frame sent
    , epWEOF :: MVar Bool -- ^ True if EOF packet sent
    , epRdone :: QSem     -- ^ Signal when receiver is done
    }

newEndpoint :: SeqNo -> Int -> IO Endpoint
newEndpoint win timeout = do
  when (win >= (maxBound `shiftR` 1)) $ return $ error "Window too large"
  ack <- newEmptySampleVar
  epsnd <- newEmptySampleVar
  lfr <- newMVar 1
  lar <- newMVar 1
  lfs <- newMVar 0
  weof <- newMVar False
  rdone <- newQSem 0
  return $ Endpoint win (timeout * 1000) ack epsnd lfr lar lfs weof rdone

killEndpoint :: Endpoint -> IO ()
killEndpoint ep = do
  modifyMVar_ (epLAR ep) $ \ack -> return $ xor ack 0x80000000

-- | Feed pure data directly to an iteratee.
inWindow :: SeqNo -> SeqNo -> SeqNo -> Bool
inWindow wsz next seqno
    | next + wsz >= next = seqno >= next && seqno < next + wsz
    | otherwise          = seqno >= next || seqno < next + wsz

-- | Warning: enumerator must be re-entrant
relSend :: forall m. (MonadIO m) =>
           Endpoint
        -> (m () -> m ThreadId)
        -> Inum L.ByteString [L.ByteString] m ()
relSend ep fork iter = doSend 0 1
    where
      inMyWindow = inWindow $ epWin ep
      doSend :: SeqNo -> SeqNo
             -> Iter L.ByteString m (Iter [L.ByteString] m ())
      doSend acked next | not $ inMyWindow acked next = do
        acked' <- liftIO $ withMVar (epLAR ep) $ \lar -> do
                    emptySampleVar (epAck ep)
                    return $ if inMyWindow (acked + 1) lar then lar else acked
        when (acked == acked') $ liftIO $ readSampleVar (epAck ep)
        doSend acked' next
      doSend acked next = do
        payload <- takeI 500
        liftIO $ modifyMVar_ (epLFS ep) $ \_ -> do
          when (L.null payload) $ modifyMVar_ (epWEOF ep) (\_ -> return True)
          liftIO $ writeSampleVar (epSnd ep) ()
          return next
        lift $ xmit next payload
        _ <- lift $ fork $ rexmit next payload
        if L.null payload
          then -- Can't return until we are done sending Acks
               liftIO (waitQSem $ epRdone ep) >> return iter
          else doSend acked (next + 1)

      xmit :: SeqNo -> L.ByteString -> m ()
      xmit seqno payload = do
        lfr <- liftIO $ readMVar (epLFR ep)
        let pkt = pktgen $ DataP lfr seqno payload
        _ <- rerun $ feedI iter $ chunk [pkt]
        return ()

      rerun :: Iter t m a -> m (Iter t m a)
      rerun (IterM m)    = m >>= rerun
      rerun (IterC _ fr) = rerun $ fr Nothing
      rerun i            = return i

      rexmit :: SeqNo -> L.ByteString -> m ()
      rexmit seqno payload = do
        liftIO (threadDelay (epTimeout ep))
        acked <- liftIO $ readMVar $ epLAR ep
        if (inMyWindow acked seqno)
          then xmit seqno payload >> rexmit seqno payload
          else return ()


relReceive :: forall m a. (MonadIO m) =>
              Endpoint
           -> Iter [L.ByteString] m ()
           -> Inum [L.ByteString] [Packet] m a
relReceive ep sender startiter = getPkts 1 Map.empty startiter
    where
      done = liftIO $ signalQSem (epRdone ep)

      getPkts :: SeqNo -> Queue -> Inum [L.ByteString] [Packet] m a
      getPkts next q iter = do
        rawpkt <- liftM (fromMaybe L.empty) safeHeadI
        case pktparse rawpkt of
          Nothing -> getPkts next q iter
          Just (AckP ackno) ->
              do _ <- recvack ackno
                 getPkts next q iter
          Just p@(DataP ackno _ _) ->
              do _ <- recvack ackno
                 doNext [] next (enqPacket next q p) iter

      doNext :: [Packet] -> SeqNo -> Queue
             -> Inum [L.ByteString] [Packet] m a
      doNext pkts next q iter =
        case Map.lookup next q of
          Just pkt -> doNext (pkt:pkts) (next+1) (Map.delete next q) iter
          Nothing | null pkts -> sendack next >> getPkts next q iter
          Nothing | otherwise ->
            do _ <- sendack next
               -- feedI (getPkts next q) pkts iter
               result <- inumMC noCtl $ feedI iter $ chunk (reverse pkts)
               case result of
                 IterF _   -> getPkts next q result
                 Done a _  -> closewait next a
                 _         -> done >> return result

      closewait :: SeqNo -> a -> Iter [L.ByteString] m (Iter [Packet] m a)
      closewait seqno a = do
        lfs <- liftIO $ takeMVar $ epLFS ep
        weof <- liftIO $ readMVar $ epWEOF ep
        liftIO $ putMVar (epLFS ep) lfs
        lar <- liftIO $ readMVar (epLAR ep)
        case () of
          () | weof && lar == lfs + 1 -> do done
                                            return $ return a
          () | lar == lfs + 1         -> do liftIO $ readSampleVar (epSnd ep) 
                                            closewait seqno a
          () | otherwise              -> closewait' seqno a

      closewait' :: SeqNo -> a -> Iter [L.ByteString] m (Iter [Packet] m a)
      closewait' seqno a = do
        rawpkt <- safeHeadI
        case liftM pktparse rawpkt of
          Nothing      -> done >> (return $ return a)
          Just Nothing -> closewait seqno a
          Just (Just (AckP ackno)) ->
              do _ <- recvack ackno
                 closewait seqno a
          Just (Just (DataP ackno _ _)) ->
              do _ <- recvack ackno
                 _ <- sendack seqno
                 closewait seqno a



      recvack :: SeqNo -> Iter [L.ByteString] m SeqNo
      recvack ackno = liftIO $ modifyMVar (epLAR ep) $ \lar ->
                      if (inMyWindow (lar + 1) ackno)
                        then do liftIO $ writeSampleVar (epAck ep) ()
                                return (ackno, ackno)
                        else return (lar, lar)
      sendack seqno = do
        liftIO $ modifyMVar_ (epLFR ep) $ \_ -> return seqno
        inumMC noCtl $ feedI sender $ chunk [pktgen $ AckP seqno]
      inMyWindow = inWindow $ epWin ep
      enqPacket next q pkt@(DataP _ seqno _) =
          if inMyWindow next seqno then Map.insert seqno pkt q else q
      enqPacket _ _ (AckP _) = error "enqPacket Ack-only packet"


--
-- Packet generation and parsing
--

cksum :: L.ByteString -> Word16
cksum binit = finish $ hi 0 binit
  where
    hi cs b | L.null b = cs
    hi cs b = lo (cs + shiftL (fromIntegral $ L.head b) 8) $ L.tail b
    lo cs b | L.null b = cs
    lo cs b            = hi (cs + (fromIntegral $ L.head b)) $ L.tail b
    finish :: Word32 -> Word16
    finish cs | cs > 0xffff = finish $ (cs .&. 0xffff) + (shiftR cs 16)
    finish 0xffff           = 0xffff
    finish cs               = complement $ fromIntegral cs

payloadSize :: Int
payloadSize = 500

-- | Add checksum and length to a packet
pktsumlen :: L.ByteString -> L.ByteString
pktsumlen asp = runPut (putWord16be cs >> putLazyByteString (L.drop 2 pkt'))
    where
      len' = 4 + L.length asp
      len | len' <= 0xffff = fromIntegral len'
          | otherwise      = error "Packet payload too large"
      pkt' = runPut (putWord16be 0 >> putWord16be len >> putLazyByteString asp)
      cs = cksum pkt'

-- | Marshall a packet
pktgen :: Packet -> L.ByteString
pktgen (AckP ackno) = pktsumlen $ runPut $ putWord32be ackno
pktgen (DataP ackno seqno payload) =
    pktsumlen $ runPut $ do
      putWord32be ackno
      putWord32be seqno
      putLazyByteString payload

pktparse :: L.ByteString -> Maybe Packet
pktparse str
    | len' < 12 && len' /= 8           = Nothing
    | len > len'                       = Nothing
    | cksum (L.take len str) /= 0xffff = Nothing
    | len == 8                         =
        runGet (skip 4 >> getWord32be >>= return . Just. AckP) str
    | otherwise                        = 
        flip runGet str $ do
          skip 4
          ackno <- getWord32be
          seqno <- getWord32be
          payload <- getLazyByteString $ len - 12
          return $ Just $ DataP ackno seqno payload
    where
      len' = L.length str
      len = fromIntegral $ runGet (skip 2 >> getWord16be) str

