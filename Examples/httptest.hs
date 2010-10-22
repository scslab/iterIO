{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (catch)
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.Socket as Net
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import System.Environment
import System.IO
import System.Posix.Files
-- import Text.XHtml.Strict

import Data.IterIO
-- import Data.IterIO.Parse
import Data.IterIO.Http
import Data.IterIO.SSL
-- import Data.IterIO.ListLike

type L = L.ByteString

data HttpServer = HttpServer {
      hsListenSock :: !Net.Socket
    , hsSslCtx :: !(Maybe SSL.SSLContext)
    , hsLog :: !(Maybe Handle)
    }

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

httpAccept :: HttpServer -> IO (Iter L IO (), Onum L IO a)
httpAccept hs = do
  (s, addr) <- Net.accept $ hsListenSock hs
  hPutStrLn stderr (show addr)
  (iter, enum) <- maybe (mkInsecure s) (mkSecure s) (hsSslCtx hs)
  return $ maybe (iter, enum |. inumhLog undefined)
                (\h -> (inumhLog h .| iter, enum |. inumhLog h))
                (hsLog hs)
  where
    mkInsecure s = do
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h `inumFinally` liftIO (hClose h))
    mkSecure s ctx = iterSSL ctx s True `catch` \e@(SomeException _) ->
                     hPutStrLn stderr (show e) >> return (nullI, return)
                  
mkServer :: Net.PortNumber -> Maybe SSL.SSLContext -> IO HttpServer
mkServer port mctx = do
  sock <- myListen port
  h <- openBinaryFile "http.log" WriteMode
  hSetBuffering h NoBuffering
  return $ HttpServer sock mctx (Just h)
  

process_request :: (MonadIO m) => HttpReq -> Iter L m (HttpResp m)
process_request req = do
  home <- liftIO $ getEnv "HOME"
  let urlpath = concatMap (('/':) . S8.unpack) (reqPathLst req)
  let path = home ++ "/.cabal/share/doc" ++ urlpath
  resp <- defaultHttpResp
  estat <- liftIO $ try $ getFileStatus path
  case estat :: Either IOError FileStatus of
    Left e -> return resp { respStatus = stat404
                          , respHeaders = map S8.pack
                                          ["Content-Type: text/plain"]
                          , respBody = enumPure $ L8.pack (show e) }
    Right stat | isDirectory stat ->
                   return resp { respStatus = stat301
                               , respHeaders = map S8.pack
                                 ["Location: " ++ urlpath ++ "/index.html"
                                 , "Content-Type: text/plain"] }
    _ -> do
       h <- liftIO $ openBinaryFile path ReadMode
       return resp { respStatus = stat200
                   , respHeaders = [S8.pack "Content-Type: text/html"]
                   , respBody = \i -> enumNonBinHandle h i
                                `finallyI` liftIO (hClose h) }

handle_connection :: Iter L IO () -> Onum L IO () -> IO ()
handle_connection iter0 enum = enum |$ reqloop iter0
    where
      reqloop iter = do
        eof <- atEOFI
        unless eof $ doreq iter
      doreq iter = do
        req <- httpreqI
        -- liftIO $ print req
        resp <- handlerI bail $
                inumHttpbody req .| (process_request req <* nullI)
        -- liftIO $ print resp
        runI (enumHttpResp resp iter) >>= reqloop
      bail e@(SomeException _) _ = do
        liftIO $ hPutStrLn stderr $ "Error: " ++ show e
        resp0 <- defaultHttpResp
        let resp = resp0 {
                     respStatus = stat500
                   , respHeaders = [S8.pack "Content-Type: text/plain"]
                   , respBody = enumPure $ L8.pack $ show e ++ "\r\n"
                   }
        liftIO $ print resp
        liftIO $ print e
        return resp

accept_loop :: HttpServer -> IO ()
accept_loop srv = loop
    where loop = httpAccept srv >>= forkIO . uncurry handle_connection >> loop

main :: IO ()
main = Net.withSocketsDo $ SSL.withOpenSSL $ do
  mctx <- if secure
          then do
            exists <- fileExist privkey
            unless exists $ genSelfSigned privkey "localhost"
            ctx <- simpleContext privkey
            return $ Just ctx
          else return Nothing
  srv <- mkServer (if secure then 4433 else 8000) mctx
  sem <- newQSem 0
  _ <- forkIO $ accept_loop srv `finally` signalQSem sem
  waitQSem sem
    where
      privkey = "testkey.pem"
      secure = True
