
module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.Socket as Net
import qualified System.IO as IO

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

testreq :: (MonadIO m) => Iter L m ()
testreq = do
  req <- httpreqI
  liftIO $ print req

main :: IO ()
main = Net.withSocketsDo $ do
         sock <- myListen port
         (s, addr) <- Net.accept sock
         print addr
         h <- Net.socketToHandle s IO.ReadWriteMode
         Net.sClose sock
         enumHandle h |. inumLog "http.log" True |$ testreq
