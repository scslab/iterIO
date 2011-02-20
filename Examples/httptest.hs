{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (catch, head, id, div)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Network.Socket as Net
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import System.Environment
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files

import Data.IterIO
-- import Data.IterIO.Parse
import Data.IterIO.Http
import Data.IterIO.HttpRoute
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
    mkSecure s ctx = iterSSL ctx s True `catch` \e@(SomeException _) -> do
                       hPutStrLn stderr (show e)
                       Net.sClose s
                       return (nullI, return)
                  
mkServer :: Net.PortNumber -> Maybe SSL.SSLContext -> IO HttpServer
mkServer port mctx = do
  sock <- myListen port
  h <- openBinaryFile "http.log" WriteMode
  hSetBuffering h NoBuffering
  return $ HttpServer sock mctx (Just h)

toPath :: [S8.ByteString] -> String
toPath = concatMap (('/':) . S8.unpack)

serve_cabal :: (MonadIO m) => HttpReq -> Iter L m (HttpResp m)
serve_cabal req = do
  home <- liftIO $ getEnv "HOME"
  let urlpath = toPath (reqPathLst req)
  let path = home ++ "/.cabal/share/doc" ++ urlpath
  estat <- liftIO $ try $ getFileStatus path
  case estat :: Either IOError FileStatus of
    Left e | isDoesNotExistError e -> return $ resp404 req
    Left e                         -> return $ resp500 $ show e
    Right stat | isDirectory stat ->
                   return $ resp301 $ toPath $
                          reqPathCtx req ++ reqPathLst req ++ ["index.html"]
    _ -> do
       h <- liftIO $ openBinaryFile path ReadMode
       return (defaultHttpResp :: HttpResp IO) {
                     respStatus = stat200
                   , respHeaders = [contentType path]
                   , respBody = enumNonBinHandle h
                                `inumFinally` liftIO (hClose h) }

fileExt :: String -> String
fileExt str = case dropWhile (/= '.') str of
                []  -> str
                _:t -> fileExt t

contentType :: String -> S8.ByteString
contentType file = S8.pack $ "Content-Type: " ++
                   case fileExt file of
                     "css"  -> "text/css"
                     "png"  -> "image/png"
                     "gif"  -> "image/gif"
                     "jpg"  -> "image/jpeg"
                     "jpeg" -> "image/jpeg"
                     "pdf " -> "application/pdf"
                     _      -> "text/html"

accept_loop :: HttpServer -> IO ()
accept_loop srv = loop
    where
      loop = do
        (iter, enum) <- httpAccept srv
        _ <- forkIO $ enum |$ inumHttpServer (ioHttpServer handler) .| iter
        loop
      handler req = fromMaybe (return $ resp404 req) $ runHttpRoute route req
      route = mconcat [ routeTop $ routeConst $ resp301 "/cabal"
                      , routeName "cabal" $ routeFn serve_cabal
                      ]

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
