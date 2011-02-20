{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpServer (mkHttpServer, runHttpServer) where

import Prelude hiding (catch, head, id, div)
import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as Net
import qualified OpenSSL.Session as SSL
import System.IO

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.SSL

type L = L.ByteString

data HttpServer = HttpServer {
      hsListenSock :: !Net.Socket
    , hsSslCtx :: !(Maybe SSL.SSLContext)
    }

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

httpAccept :: HttpServer -> IO (Net.SockAddr, Iter L IO (), Onum L IO a)
httpAccept hs = do
  (s, addr) <- Net.accept $ hsListenSock hs
  (iter, enum) <- maybe (mkInsecure s) (mkSecure s) (hsSslCtx hs)
  return (addr, iter, enum)
  where
    mkInsecure s = do
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h `inumFinally` liftIO (hClose h))
    mkSecure s ctx = iterSSL ctx s True `catch` \e@(SomeException _) -> do
                       hPutStrLn stderr (show e)
                       Net.sClose s
                       return (nullI, return)

mkHttpServer :: Net.PortNumber -> Maybe SSL.SSLContext -> IO HttpServer
mkHttpServer port mctx = do
  sock <- myListen port
  return $ HttpServer { hsListenSock = sock
                      , hsSslCtx = mctx
                      }

runHttpServer :: (MonadIO m)
              => HttpServer
              -> HttpRequestHandler m
              -> (Net.SockAddr -> m (Iter L m ()) -> IO (Iter L m ()))
              -> IO ()
runHttpServer srv handler runConn = forever $ do
  (addr, iter, enum) <- httpAccept srv
  let processConnection = inumHttpServer $ ioHttpServer handler
  _ <- forkIO $ enum |$ adaptIterM (runConn addr) (processConnection .| liftIterIO iter)
  return ()

