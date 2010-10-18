{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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

secure :: Bool
secure = False

port :: Net.PortNumber
port = if secure then 4443 else 8000

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

process_request :: (MonadIO m) => HttpReq -> Iter L m (HttpResp m)
process_request req = do
  home <- liftIO $ getEnv "HOME"
  let path = home ++ "/.cabal/share/doc" ++
             concatMap (('/':) . S8.unpack) (reqPathLst req)
  resp <- defaultHttpResp
  h <- liftIO $ openBinaryFile path ReadMode
  return resp { respStatus = stat200
              , respHeaders = [S8.pack "Content-Type: text/html"]
              , respBody = \i -> enumNonBinHandle h i
                           `finallyI` liftIO (hClose h) }

handle_connection :: Onum L IO () -> Iter L IO () -> IO ()
handle_connection enum iter0 = enum |$ reqloop iter0
    where
      reqloop iter = do
        eof <- atEOFI
        unless eof $ do
          req <- httpreqI
          iter' <- handlerI (bail iter) $ do
            resp <- inumHttpbody req .| (process_request req <* nullI)
            runI $ enumHttpResp resp $ iter
          reqloop iter'
      bail :: Iter L IO () -> SomeException
           -> Iter L IO (Iter L IO ())
           -> Iter L IO (Iter L IO ())
      bail iter e@(SomeException _) _ = do
        liftIO $ putStrLn "I'm bailing"
        resp0 <- defaultHttpResp
        let resp = resp0 {
                     respStatus = stat500
                   , respHeaders = [S8.pack "Content-Type: text/plain"]
                   , respBody = enumPure $ L8.pack $ show e ++ "\r\n"
                   }
        liftIO $ print resp
        _ <- runI $ enumHttpResp resp stdoutI
        runI $ enumHttpResp resp iter

accept_loop :: SSL.SSLContext -> QSem -> Net.Socket -> IO ()
accept_loop ctx sem sock = do
  (s, addr) <- Net.accept sock
  print addr
  (iter, enum) <- if secure
      then sslFromSocket ctx s True
      else do h <- Net.socketToHandle s ReadWriteMode
              hSetBuffering h NoBuffering
              return (handleI h,
                      \i -> enumHandle h i `finallyI` liftIO (hClose h))
  liftIO $ putStrLn "got this far"
  -- _ <- forkIO $
  handle_connection (enum |. inumLog "request.log" False)
                    (inumLog "response.log" False .| iter)
  accept_loop ctx sem sock

privkey :: FilePath
privkey = "testkey.pem"

main :: IO ()
main = Net.withSocketsDo $ SSL.withOpenSSL $ do
         sock <- myListen port
         sem <- newQSem 0
         exists <- fileExist privkey
         unless exists $ genSelfSigned "testkey.pem" "localhost"
         ctx <- simpleContext "testkey.pem"
         accept_loop ctx sem sock
         waitQSem sem
         Net.sClose sock

-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
