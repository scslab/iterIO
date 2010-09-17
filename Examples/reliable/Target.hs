{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Rank2Types #-}

module Target where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Data.Word
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L (defaultChunkSize)
import Foreign.C.Types
import Network.Socket
import System.IO
import System.IO.Error
import System.Posix.Types
import System.Process
import System.Process.Internals (ProcessHandle__(..)
                                , withProcessHandle
                                , withProcessHandle_)
import Text.Regex.Posix
import Text.Printf

-- import Debug.Trace

import Data.IterIO
import Data.IterIO.Extra (iterLoop)
import Data.IterIO.Base (run)

import Arc4
import TM
import Protocol

foreign import ccall "signal.h kill" c_kill :: CPid -> CInt -> IO CInt
foreign import ccall unsafe "arpa/inet.h htonl" htonl :: Word32 -> Word32

-- Stupid Haskell won't let you connect after binding
connect_fix :: Socket -> SockAddr -> IO ()
connect_fix s@(MkSocket _ _ _ _ status) addr = do
  modifyMVar_ status $ \st -> return $ if st == Bound then NotConnected else st
  connect s addr

data Target a b = Target {
      tUSource :: Onum [L.ByteString] TM a
    , tUDrain :: Iter [L.ByteString] TM ()
    , tSource :: Onum L.ByteString TM b
    , tDrain :: Iter L.ByteString TM ()
    , tKill :: IO ()
    }
type Targ = Target () Bool

{-
enumConcat :: (Monad m, Monoid t) => Inum [t] t m a
enumConcat iter = do
  next <- safeHeadI
  case next of
    Nothing -> return iter
    Just t -> feedI enumConcat t iter
-}

pktshow :: Packet -> String
pktshow (AckP ackno) = printf "Ack  ack = %08x" ackno
pktshow (DataP ackno seqno payload) =
    (printf "Data ack = %08x, seq = %08x, data = " ackno seqno
            :: String)
           ++ show (if L.length payload > 20
                    then L8.unpack (L.take 20 payload) ++ "..."
                    else L8.unpack payload)

pktDebug :: (MonadIO m) => String -> Inum [L.ByteString] [L.ByteString] m a
pktDebug prefix = mkInum $ do
  raw <- headI
  case pktparse raw of
    Nothing  -> liftIO $ S8.hPut stderr
                $ S8.pack $ prefix ++ "corrupt packet\n"
    Just pkt -> liftIO $ S8.hPut stderr $ S8.pack
                $ prefix ++ pktshow pkt ++ "\n"
  return [raw]

{-
pktPut :: (Monad m) => Inum [Packet] L.ByteString m a
pktPut iter = safeHeadI >>= process
    where
      process Nothing = return iter
      process (Just (DataP _ _ payload))
          | L.null payload = return iter
          | otherwise      = feedI pktPut payload iter
      process _            = pktPut iter
-}

pktPut :: (Monad m) => Inum [Packet] L.ByteString m a
pktPut = mkInum $ headI >>= process
    where
      process (DataP _ _ payload)
          | L.null payload = throwEOFI "EOF"
          | otherwise      = return payload
      process _ = headI >>= process

internalTarget :: TM (Target a b)
internalTarget = do
  tc <- ask
  ep <- liftIO $ newEndpoint (tcWin tc) (tcTimeout tc)
  (dgramI, dgramE) <- if tcDebug tc
                      then do (dgramI', dgramE') <- iterLoop
                              return (pktDebug "send " .| dgramI',
                                      dgramE' |. pktDebug "recv ")
                      else iterLoop
  (streamI, streamE) <- iterLoop
  return $ Target dgramE
             (relReceive ep dgramI .| pktPut .| streamI)
             streamE
             (relSend ep forkTM .| dgramI)
             (liftIO $ killEndpoint ep)

phPid :: ProcessHandle -> IO CPid
phPid ph = withProcessHandle ph $ \p ->
          case p of
            OpenHandle pid -> return (p, pid)
            _              -> return (p, -1)


kill :: ProcessHandle -> CInt -> IO ()
kill ph sig = do
  withProcessHandle_ ph $ \p ->
      case p of
        OpenHandle pid -> c_kill pid sig >> return p
        _              -> return p
  _ <- forkIO $ waitForProcess ph >> return ()
  return ()


startServer :: TM ServerProc
startServer = do
  targ <- asks tcTarget
  quiet <- asks tcQuiet
  win <- asks tcWin
  gdb <- asks tcGdb
  timeout <- asks tcTimeout
  liftIO $ do
    (errS, errS') <- socketPair AF_UNIX Stream 0
    shutdown errS ShutdownSend
    (err, err') <- s2h2 $ return (errS, errS')
    tcpSock <- socket AF_INET Stream 0
    bindSocket tcpSock $ SockAddrInet 0 iNADDR_ANY
    listen tcpSock 5
    (SockAddrInet myPort _) <- getSocketName tcpSock
    (_, _, _, ph) <- createProcess $
                     (proc (head targ) $
                           (tail targ) ++ ["-s", "-w", show win
                                          , "-t", show timeout
                                          , "0", "127.0.0.1:" ++ show myPort])
                     { cwd = Nothing, env = Nothing
                     , std_err = UseHandle err', close_fds = True }
    peer <- doErr quiet err
    when gdb $ notify (head targ) ph
    return $ ServerProc peer tcpSock (kill ph 15 >> sClose tcpSock)

enumAccept :: (MonadIO m) => Socket -> MVar Handle -> Onum L.ByteString m a
enumAccept sock mv iter = do
  (s, _) <- liftIO $ accept sock
  h <- liftIO $ socketToHandle s ReadWriteMode
  liftIO $ hSetBuffering h NoBuffering
  ok <- liftIO $ tryPutMVar mv h
  unless ok $ error "enumAccept:  MVar full"
  enumHandle h iter

iterMVH :: (MonadIO m) => MVar Handle -> Iter L.ByteString m ()
iterMVH mvh = iterF $ \c -> do h <- liftIO $ readMVar mvh
                               feedI (handleI h) c

spawnOrConnect :: TM (Target a b)
spawnOrConnect = do
  mserver <- asks tcServer
  case mserver of
    Nothing -> spawnTarget
    Just server ->
        do udpSock <- liftIO $ socket AF_INET Datagram 0
           liftIO $ connect udpSock $ spAddr server
           mvh <- liftIO $ newEmptyMVar
           return $ Target (enumDgram udpSock)
                      (sendI $ liftIO . sendStr udpSock)
                      (enumAccept (spListenSock server) mvh)
                      (iterMVH mvh)
                      (sClose udpSock)

spawnTarget :: TM (Target a b)
spawnTarget = do
  targ <- asks tcTarget
  quiet <- asks tcQuiet
  win <- asks tcWin
  gdb <- asks tcGdb
  timeout <- asks tcTimeout
  liftIO $ do
    (inout, inout') <- s2h2 $ socketPair AF_UNIX Stream 0
    (errS, errS') <- socketPair AF_UNIX Stream 0
    shutdown errS ShutdownSend
    (err, err') <- s2h2 $ return (errS, errS')
    udpSock <- socket AF_INET Datagram 0
    bindSocket udpSock $ SockAddrInet 0 iNADDR_ANY
    (SockAddrInet myPort _) <- getSocketName udpSock
    (_, _, _, ph) <- createProcess $
                     (proc (head targ) $
                           (tail targ) ++ ["-w", show win
                                          , "-t", show timeout
                                          , "0", "127.0.0.1:" ++ show myPort])
                     { cwd = Nothing, env = Nothing, std_in = UseHandle inout'
                     , std_out = UseHandle inout', std_err = UseHandle err'
                     , close_fds = True }
    hClose inout'
    hClose err'
    peer <- doErr quiet err
    connect_fix udpSock peer
    hSetBuffering inout NoBuffering
    when gdb $ notify (head targ) ph
    return $ Target (enumDgram udpSock) (sendI $ liftIO . sendStr udpSock)
              (enumHandle inout) (handleI inout)
              (kill ph 15 >> hClose inout >> sClose udpSock)

notify :: String -> ProcessHandle -> IO ()
notify targ ph = do
          pid <- phPid ph
          hPutStrLn stderr $ "\nSpawned " ++ targ ++ " with PID "
                        ++ show pid ++ ".  Press RETURN to continue."
          _ <- hWaitForInput stdin (-1)
          _ <- L.hGetNonBlocking stdin 512
          return ()

s2h2 :: IO (Socket, Socket) -> IO (Handle, Handle)
s2h2 m = do (s1, s2) <- m
            h1 <- socketToHandle s1 ReadWriteMode
            h2 <- socketToHandle s2 ReadWriteMode
            return (h1, h2)

doErr :: Bool -> Handle -> IO SockAddr
doErr quiet' h' = getPort quiet' h' []
    where
      getPort quiet h acc =
          do buf <- getSome h
             unless quiet $ sendBuf buf
             let acc' = acc ++ L8.unpack buf
             case acc' =~ "UDP port ([0-9]+)[^0-9]"
                       :: (String, String, String, [String]) of
               (_,_,_,p:_) -> do _ <- forkIO $ handle eofHandler $
                                        (if quiet then drainHandle
                                         else copyHandle)
                                        h `finally` hClose h
                                 return $ SockAddrInet
                                            (fromIntegral (read p :: Int))
                                            (htonl 0x7f000001)
               _           -> getPort quiet h acc'
      getSome h = hWaitForInput h (-1) >> L8.hGetNonBlocking h 8192
      eofHandler e = if isEOFError e then return () else ioError e
      copyHandle h = getSome h >>= sendBuf >> copyHandle h
      drainHandle h = getSome h >> drainHandle h
      sendBuf = L8.hPut stderr

lS :: Int -> L.ByteString
lS n | n <= 0 = L.empty
     | n < linelen = L8.pack $ replicate (n-1) '.' ++ "\n"
     | otherwise = 
         let msg = show n ++ " more characters to go"
             msgpad = msg ++ (replicate ((linelen - length msg) - 1) ' ')
         in L8.append (L8.pack (msgpad ++ "\n")) $ lS (n - linelen)
    where
      linelen = 47

expectI :: (Monad m) => L.ByteString -> Iter L.ByteString m Bool
expectI goal | L.null goal = return True
             | otherwise = do
  actual <- takeI $ fromIntegral L.defaultChunkSize
  if L.isPrefixOf actual goal
    then expectI $ L.drop (L.length actual) goal
    else return False

lE :: (Monad m) => Int -> Onum L.ByteString m a
lE n0 = mkInumM $ codec n0
    where
      codec n
          | n <= 0      = return ()
          | n < linelen = ret n (replicate (n-1) '.')
          | otherwise =
              let msg = show n ++ " more characters to go"
                  msgpad = msg ++ (replicate ((linelen - length msg) - 1) ' ')
              in ret n msgpad
      linelen = 47
      ret n str = do done <- ifeed (L8.pack $ str ++ "\n")
                     when (not done && n > linelen) $ codec $ n - linelen


packE :: (Monad m) => Inum L.ByteString L.ByteString m a
packE = mkInum $ do
  packet' <- takeExactI $ fromIntegral L.defaultChunkSize
  let packet = L.fromChunks [S8.concat $ L.toChunks packet']
  return packet

cE :: (Monad m) => Char -> Int -> Onum L.ByteString m a
cE c len = inumPure $ L8.replicate (fromIntegral len) c
nI :: (Monad m) => Int -> Iter L.ByteString m Bool
nI n = do
  s <- takeI $ fromIntegral n
  case n - (fromIntegral $ L.length s) of
    0            -> return True
    n' | n == n' -> return False
    n'           -> nI n'

a4E :: (Monad m) => String -> Int -> Onum L.ByteString m a
a4E key len = mkInumM $ enumChunks (a4new key) len
    where
      enumChunks a4 n
          | n <= 0    = return ()
          | otherwise =
              let chunklen = min n L.defaultChunkSize
                  (c, a4') = a4chunk a4 chunklen
              in do done <- ifeed (L.fromChunks [c])
                    unless done $ enumChunks a4' (n - chunklen)

a4I :: (MonadIO m) => String -> Int -> Iter L.ByteString m Bool
a4I key len = iter (a4new key) len
    where
      iter a4 n
          | n == 0    = return True
          | otherwise = -- traceShow n $
              do bytes <- takeI $ fromIntegral n
                 -- liftIO $ putTraceMsg $ "got " ++ show (L.length bytes)
                 if L.length bytes == 0
                   then return False
                   else do let chunklen = fromIntegral $ L.length bytes
                               (c, a4') = a4chunk a4 $ fromIntegral chunklen
                           if bytes /= L.fromChunks [c]
                             then return False
                             else iter a4' (n - chunklen)

strTestLen :: Int
-- strTestLen = 262144
strTestLen = 65536

keyGen :: TM String
keyGen = liftM L8.unpack $ asks tcRnd >>= flip a4RandomStringN 16


runStreamTest :: [ThreadId]
              -> [Target () Bool]
              -> Int
              -> [TM Bool]
              -> [TM ()]
              -> TM Bool
runStreamTest udpthreads eps timeout tests others = do
  let ntests = length tests
  numok <- liftIO $ newMVar 0
  done <- liftIO $ newQSemN 0
  tt <- forkTests numok done tests
  ot <- mapM forkTM others
  to <- (liftIO $ forkIO $ threadDelay (timeout * 1000000)
                    >> signalQSemN done ntests)
  liftIO $ do waitQSemN done ntests
              ok <- readMVar numok
              _ <- mapM killThread $ to:(tt ++ ot ++ udpthreads)
              _ <- mapM tKill eps
              return $ ok == ntests
  where
    inc x = return $ x + 1
    forkTests :: MVar Int -> QSemN -> [TM Bool] -> TM [ThreadId]
    forkTests _ _ []                 = return []
    forkTests numok done (test:rest) = do
      tid <- forkTM $ do res <- test
                         when res $ liftIO $ modifyMVar_ numok inc
                         liftIO $ signalQSemN done 1
      tids <- forkTests numok done rest
      return $ tid:tids
      
oneWay :: [ThreadId] -> Targ -> Targ -> TM Bool
oneWay ut a b = do
  key <- keyGen
  runStreamTest ut [a, b] 60
             [tSource a |$ (nullI >> return True)
             , tSource b |$ a4I key strTestLen]
             [a4E key strTestLen |$ tDrain a
             , run $ tDrain b
             ]

twoWay :: [ThreadId] -> Targ -> Targ -> TM Bool
twoWay ut a b = do
  key1 <- keyGen
  key2 <- keyGen
  runStreamTest ut [a, b] 60
             [tSource b |$ a4I key1 strTestLen
             , tSource a |$ a4I key2 strTestLen]
             [a4E key1 strTestLen |$ tDrain a
             , a4E key2 strTestLen |$ tDrain b]

pingPong :: [ThreadId] -> Targ -> Targ -> TM Bool
pingPong ut a b = do
  runStreamTest ut [a, b] 30
               [inumPure (L8.pack "1000\n") `cat` tSource a
                    |$ minusOne 1000 (tDrain a)
               , tSource b |$ minusOne 999 (tDrain b)]
               []
    where
      minusOne :: (Monad m) =>
                  Int
               -> Iter L.ByteString m a
               -> Iter L.ByteString m Bool
      minusOne expect iter = do
        line <- lineI
        case reads $ L8.unpack line of
          (n, []):_ | n == expect && n == 1 ->
               do _ <- inumMC noCtl $ feedI iter $ chunk  (L8.pack "0\n")
                  return True
          (n, []):_ | n == expect && n == 0 -> return True
          (n, []):_ | n == expect ->
               do r <- inumMC noCtl $ feedI iter $
                       chunk (L8.pack $ (show $ n - 1) ++ "\n")
                  minusOne (expect - 2) $ r
          _ -> return False

flowControl :: [ThreadId] -> Target () Bool -> Target () Bool -> TM Bool
flowControl ut a b = do
  key <- keyGen
  liftM not $ runStreamTest ut [a, b] 5
                [a4E key 0x200000 |$ tDrain a >> return True]
                [run (tDrain b)]



eofTest :: [ThreadId] -> Target () Bool -> Target () Bool -> TM Bool
eofTest ut a b = do
  key <- keyGen
  let str = a4stringN key strTestLen
  -- let str = lS strTestLen
  runStreamTest ut [a, b] 10
              [tSource a |$ a4I key strTestLen]
              [tSource b |$ (nullI >> lift (sendit str) >> return True)
                   >> return ()
              , inumPure (L8.pack "request\n") |$ tDrain a]
    where
      sendit str = inumPure str |$ (tDrain b >> return True)
{-
              [tSource a |$ a4I key strTestLen]
              [tSource b |$ (nullI >> lift (a4E key strTestLen |$ packE
                                            .| (tDrain b >> return True)))
                       >> return ()
              , inumPure (L8.pack "request\n") |$ tDrain a]
-}
