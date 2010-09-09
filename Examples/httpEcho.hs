
module Main (main) where

import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.Socket as Net
import qualified System.IO as IO

import Data.IterIO
import Data.IterIO.Http
import qualified Data.ListLike as LL

import Text.XHtml.Strict

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

req2XHtml :: (MonadIO m) => Inum L [Html] m a
req2XHtml = mkInum' $ do
  req <- httpreqI
  let reqS = show req
  liftIO $ IO.hPutStrLn IO.stderr reqS
  return [stringToHtml reqS]

xhtml2L :: (MonadIO m) => Inum [Html] L m a
xhtml2L = mkInum' $ do
  h <- headLI
  return $ L.append (headersL xhtmlHeaders)
                    (L.pack $ showHtml h)  -- UTF-8?

xhtmlHeaders :: [String]
xhtmlHeaders = [ "HTTP/1.1 200 OK", "Content-type: text/xhtml" ]

headersL :: [String] -> L
headersL hh = L.append (L.concat (map ((flip L.append crlf) . L.pack) hh)) crlf

crlf :: L
crlf = L.pack ['\r', '\n']

stderrLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) => Inum t t m a
stderrLog = inumhLog IO.stderr

main :: IO ()
main = Net.withSocketsDo $ do
         sock <- myListen port
         (s, addr) <- Net.accept sock
         print addr
         h <- Net.socketToHandle s IO.ReadWriteMode
         Net.sClose sock
         enumHandle' h |. stderrLog
            |$ req2XHtml .| xhtml2L .| stderrLog .| handleI h
