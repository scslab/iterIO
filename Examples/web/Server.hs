module Server (server) where

import           Control.Concurrent
import           Control.Exception (finally)
import           Control.Monad (forever)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Socket as Net
import qualified System.IO as IO

--
-- Server
--

data Connection = Connection { cxTid :: ThreadId }
type Connections = MVar (Map ThreadId Connection)
data Event = CxDone ThreadId
           | CxNote ThreadId String
type Events = Chan Event

server :: Net.PortNumber -> (IO.Handle -> IO ()) -> IO ()
server port handleRequest = Net.withSocketsDo $ do
  listener <- myListen port
  connections <- newMVar Map.empty
  events <- newChan
  _ <- forkIO $ forever $ acceptConnection listener connections events
  forever $ handleEvent connections events

 where
  acceptConnection :: Net.Socket -> Connections -> Events -> IO ()
  acceptConnection listener connections events = do
    (s, addr) <- Net.accept listener
    c <- spawnConnection s addr events
    modifyMVar_ connections $ return . Map.insert (cxTid c) c

  handleEvent :: Connections -> Events -> IO ()
  handleEvent connections events = do
    event <- readChan events
    case event of
      CxNote tid msg ->
        warn $ show tid ++ ": " ++ msg
      CxDone tid -> do
        modifyMVar_ connections $ return . Map.delete tid
        warn $ show tid ++ " FINISHED"

  spawnConnection :: Net.Socket -> Net.SockAddr -> Events -> IO Connection
  spawnConnection s addr events = do
    tid <- forkIO $ do
              tid <- myThreadId
              handleConnection s `finally` writeChan events (CxDone tid)
    writeChan events $
      CxNote tid $ "Handling connection from " ++ show addr
    return $ Connection tid

  handleConnection :: Net.Socket -> IO ()
  handleConnection s = do
    h <- Net.socketToHandle s IO.ReadWriteMode
    IO.hSetBuffering h IO.NoBuffering
    handleRequest h
    IO.hClose h


--
-- Utilities
--

warn :: String -> IO ()
warn msg = IO.hPutStrLn IO.stderr msg

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock


