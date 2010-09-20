
module Main where

-- import Control.Monad.Trans
import Control.Concurrent
import Control.Exception (finally)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.Socket as Net
import System.IO
-- import Text.XHtml.Strict

import Data.IterIO
-- import Data.IterIO.Parse
import Data.IterIO.Http

type L = L.ByteString

port :: Net.PortNumber
port = 8000

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

handle_connection :: Handle -> IO ()
handle_connection h = do
  req <- enumHandle h |. inumLog "http.log" True |$ httpreqI
  print req

accept_loop :: QSem -> Net.Socket -> IO ()
accept_loop sem sock = do
  (s, addr) <- Net.accept sock
  print addr
  h <- Net.socketToHandle s ReadWriteMode
  _ <- forkIO $ handle_connection h `finally` hClose h
  accept_loop sem sock

main :: IO ()
main = Net.withSocketsDo $ do
         sock <- myListen port
         sem <- newQSem 0
         accept_loop sem sock
         waitQSem sem
         Net.sClose sock

