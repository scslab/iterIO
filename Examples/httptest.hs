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
import System.Directory (getAppUserDataDirectory)
import System.IO.Unsafe (unsafePerformIO)

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

routeFS :: (MonadIO m) => FilePath -> HttpRoute m
routeFS = routeFileSys mimeMap (dirRedir "index.html")

cabal_dir :: String
cabal_dir = (unsafePerformIO $ getAppUserDataDirectory "cabal") ++ "/share/doc"

serve_cabal :: (MonadIO m) => HttpRoute m
serve_cabal = routeFS cabal_dir

route :: (MonadIO m) => HttpRoute m
route = mconcat
        [ routeTop $ routeConst $ resp301 "/cabal"
        , routeMap' [ ("cabal", routeConst $ resp301 cabal_dir)
                    , ("static", routeFS "static") -- directory ./static
                    ]
        , routePath cabal_dir $ routeFS cabal_dir
        , routePath "/usr/share/doc/ghc/html" $
                    routeFS "/usr/share/doc/ghc/html"
        ]

accept_loop :: HttpServer -> IO ()
accept_loop srv = loop
    where
      loop = do
        (s, addr) <- Net.accept $ hsListenSock srv
        hPutStrLn stderr (show addr)
        _ <- forkIO $ server s
        loop
      server s = do
        (iter, enum) <- maybe (iterStream s) (\ctx -> iterSSL ctx s True)
                        (hsSslCtx srv)
        let loger = maybe inumNop inumhLog $ hsLog srv
        enum |. loger |$ inumHttpServer (ioHttpServer handler) .| loger .| iter
      handler = runHttpRoute route

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
