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
import Data.Monoid
import qualified Network.Socket as Net
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import System.IO
import System.Posix.Files

import Data.IterIO
-- import Data.IterIO.Parse
import Data.IterIO.Http
import Data.IterIO.HttpRoute
import Data.IterIO.SSL
import           System.Directory (getAppUserDataDirectory)
import           System.IO.Unsafe (unsafePerformIO)

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
  return $ maybe (iter, enum |. inumNop)
                (\h -> (inumhLog h .| iter, enum |. inumhLog h))
                (hsLog hs)
  where
    mkInsecure s = do
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h `inumFinally` liftIO (hClose h))
    mkSecure s ctx = iterSSL ctx s True `catch` \e@(SomeException _) -> do
                       hPutStrLn stderr ("iterSSL: " ++ show e)
                       return (nullI, inumNull)
                  
mkServer :: Net.PortNumber -> Maybe SSL.SSLContext -> IO HttpServer
mkServer port mctx = do
  sock <- myListen port
  h <- openBinaryFile "http.log" WriteMode
  hSetBuffering h NoBuffering
  return $ HttpServer sock mctx (Just h)

mimeMap :: String -> S8.ByteString
mimeMap = unsafePerformIO $ do
            path <- findMimeTypes ["mime.types"
                                  , "/etc/mime.types"
                                  , "/var/www/conf/mime.types"]
            enumFile path |$ mimeTypesI "application/octet-stream"
    where
      findMimeTypes (h:t) = do exist <- fileExist h
                               if exist then return h else findMimeTypes t
      findMimeTypes []    = return "mime.types" -- cause error

cabal_dir :: String
cabal_dir = (unsafePerformIO $ getAppUserDataDirectory "cabal") ++ "/share/doc"

serve_cabal :: (MonadIO m) => HttpRoute m
serve_cabal = routeFileSys mimeMap (dirRedir "index.html") cabal_dir

accept_loop :: HttpServer -> IO ()
accept_loop srv = loop
    where
      loop = do
        (iter, enum) <- httpAccept srv
        _ <- forkIO $ enum |$ inumHttpServer (ioHttpServer handler) .| iter
        loop
      handler = runHttpRoute route
      route = mconcat [ routeTop $ routeConst $ resp301 "/cabal"
                      , routeName "cabal" $ serve_cabal
                      , routePath cabal_dir $ serve_cabal
                      , routePath "/usr/share/doc/ghc/html"
                        $ routeFileSys mimeMap (const mempty)
                              "/usr/share/doc/ghc/html"
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
