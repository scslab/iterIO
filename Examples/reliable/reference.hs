{-
   This is a reference implementation in Haskell of a reliable
   transport over UDP assignment used in Stanford's CS144 class.
   See this web page for the original assignment:

   http://www.scs.stanford.edu/09au-cs144/lab/reliable.html
-}

module Main where

import Control.Concurrent
-- import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Trans
-- import Data.Maybe
-- import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Network.Socket
import System.Console.GetOpt
import System.Environment
import System.Exit
-- import System.FilePath
import System.IO

import Data.IterIO
import Data.IterIO.Extra

import Protocol

-- For some strange reason Haskell won't let you connect after binding
connect_fix :: Socket -> SockAddr -> IO ()
connect_fix s@(MkSocket _ _ _ _ status) addr = do
  modifyMVar_ status $ \st -> return $ if st == Bound then NotConnected else st
  connect s addr


{-
pktPrint :: Inum [Packet] [Packet] IO ()
pktPrint iter = do
  pkt <- headI
  liftIO $ hPutStrLn stderr $ show pkt
  feedI pktPrint [pkt] iter
-}

{-
rawPktPrint :: String -> Inum [L.ByteString] [L.ByteString] IO ()
rawPktPrint prefix iter = do
  mraw <- safeHeadI
  case mraw of
    Nothing -> return iter
    Just raw ->
        do case pktparse raw of
             Nothing  -> return iter
             Just pkt ->
                 do liftIO (hPutStrLn stderr $ prefix ++ show pkt)
                    feedI (rawPktPrint prefix) [raw] iter
-}

rawPktPrint :: String -> Inum [L.ByteString] [L.ByteString] IO ()
rawPktPrint prefix = mkInum dopkt
    where
      dopkt = do
        raw <- headI
        do case pktparse raw of
             Nothing  -> dopkt
             Just pkt -> do liftIO (hPutStrLn stderr $ prefix ++ show pkt)
                            return [raw]

pktPut :: Iter [Packet] IO ()
pktPut = safeHeadI >>= process
    where
      process Nothing = return ()
      process (Just (DataP _ _ payload))
          | L.null payload = liftIO $ hShutdown stdout 1 >> return ()
          | otherwise      = liftIO (L.hPut stdout payload) >> pktPut
      process _            = pktPut

sendStr :: (SendRecvString t) => Socket -> t -> IO Int
sendStr s t = genSendTo s t Nothing

test :: Bool -> Int -> SeqNo -> SockAddr -> SockAddr -> IO ()
test d t w l r = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  sock <- socket AF_INET Datagram 0
  bindSocket sock l
  port <- getSocketName sock >>= return . portOfAddr
  hPutStrLn stderr $ "[listening on UDP port " ++ show port ++ "]"
  connect_fix sock r
  ep <- newEndpoint w t
  let sender' = sendI $ liftIO . sendStr sock
      sender = if d then rawPktPrint "send " .| sender' else sender'
      receiver' = enumDgram sock
      receiver = if d then receiver' |. rawPktPrint "recv " else receiver'

  reader <- newEmptyMVar

  let recvpipe = receiver |$ relReceive ep sender .| pktPut
  -- forkIO $ (recvpipe `finally` putMVar reader () >> forever recvpipe)
  _ <- forkIO $ (recvpipe `finally` putMVar reader ())

  enumHandle stdin |$ relSend ep forkIO .| sender
  takeMVar reader

parseDest :: String -> (Maybe String, String)
parseDest dest =
    let (vrs, tsoh) = break (== ':') $ reverse dest
        srv = reverse vrs
    in case tsoh of
         (':':h) -> (Just $ reverse h, srv)
         _       -> (Nothing, srv)

getHost :: Bool -> String -> IO SockAddr
getHost passive dest = do
  let (host, srv) = parseDest dest
  let hints = defaultHints { addrFlags = if passive then[AI_PASSIVE] else []
                           , addrFamily = AF_INET
                           , addrSocketType = Datagram
                           }
  ai <- getAddrInfo (Just hints) host (Just srv)
  return $ addrAddress $ head ai

data Options = Options{ optTimeout :: Int
                      , optWindow :: SeqNo
                      , optDebug :: Bool
                      }
defaultOptions :: Options 
defaultOptions = Options { optTimeout = 2000
                         , optWindow = 1
                         , optDebug = False
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "w" ["window"]
      (ReqArg (\n o -> o { optWindow = read n }) "SIZE")
       ("sliding window size (default "
        ++ show (optWindow defaultOptions) ++ ")")
    , Option "t" ["timeout"]
      (ReqArg (\n o -> o { optWindow = read n }) "msec")
       ("retransmission timeout (default "
        ++ show (optTimeout defaultOptions) ++ ")")
    , Option "d" ["debug"]
      (NoArg (\o -> o { optDebug = True }))
       "print packets"
    ]

doOpt :: IO (Options, [String])
doOpt = do
  argv <- getArgs
  case getOpt RequireOrder options argv of
    (o,n,[]) -> return $ (foldl (flip ($)) defaultOptions o, n)
    (_,_,errs) -> do
          hPutStrLn stderr $ concat errs
          usage

usage :: IO a
usage = do
  prog <- getProgName
  let header = "usage: " ++ prog ++ " [OPTIONS...] udp-port [host:]udp-port\n"
  hPutStrLn stderr $ usageInfo header options
  exitFailure

main :: IO ()
main = withSocketsDo $ do
  (o, argv) <- doOpt
  unless (length argv == 2) usage
  l <- getHost True $ argv !! 0
  r <- getHost False $ argv !! 1
  test (optDebug o) (optTimeout o) (optWindow o) l r
