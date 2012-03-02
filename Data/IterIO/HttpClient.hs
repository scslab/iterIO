
module Data.IterIO.HttpClient where

import Prelude hiding (catch, head, id, div)

import Control.Exception
import Control.Monad.Trans

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Network.Socket as Net
import qualified Network.BSD as Net
import qualified OpenSSL.Session as SSL

import System.IO

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.SSL

type L = L.ByteString
type S = S.ByteString

data HttpClient = HttpClient {
      hcSock     :: !Net.Socket
    , hcSockAddr :: !Net.SockAddr
    , hcSslCtx   :: !(Maybe SSL.SSLContext)
    }

-- | Given an HTTP client configuration, make the actual connection to
-- server.
httpConnect :: HttpClient -> IO (Iter L IO (), Onum L IO a)
httpConnect hc = do
  let s = hcSock hc
  Net.connect s (hcSockAddr hc)
  (iter, enum) <- maybe (mkInsecure s) (mkSecure s) (hcSslCtx hc)
  return (iter, enum)
  where
    mkInsecure s = do
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h `inumFinally` liftIO (hClose h))
    mkSecure s ctx = iterSSL ctx s False `catch` \e@(SomeException _) -> do
                       hPutStrLn stderr (show e)
                       Net.sClose s
                       return (nullI, inumNull)


-- | Some of this code is from the "HTTP" package.
mkHttpClient :: S -> Int -> Maybe SSL.SSLContext -> IO HttpClient
mkHttpClient host port ctx = withSocket $ \s -> do
    Net.setSocketOption s Net.KeepAlive 1
    hostA <- getHostAddr (S8.unpack host)
    let a = Net.SockAddrInet (toEnum port) hostA
    return HttpClient { hcSock = s
                      , hcSockAddr = a
                      , hcSslCtx = ctx
                      }
 where withSocket action = do
         s <- Net.socket Net.AF_INET Net.Stream 6
         catchIO (action s) (\e -> Net.sClose s >> ioError e)
       getHostAddr h = do
         catchIO (Net.inet_addr h) $ \_ -> do
           h' <- getHostByName_safe h
           case Net.hostAddresses h' of
             []     -> err $ "No addresses in host entry for " ++ show h
             (ha:_) -> return ha
       getHostByName_safe h = 
         catchIO (Net.getHostByName h) $ \_ ->
           err $ "Failed to lookup " ++ show h
       err = throwIO . userError

simpleHttp :: String -> Maybe SSL.SSLContext -> IO (HttpResp IO)
simpleHttp urlString ctx = do
  req <- getRequest (L8.pack urlString)
  port <- maybe (defaultPort $ reqScheme req) return $ reqPort req
  let cEnum  = enumHttpReq req
  client <- mkHttpClient (reqHost req) port ctx
  (sIter,sEnum) <- httpConnect client
  cEnum |$ sIter
  sEnum |$ httpRespI 
    where defaultPort s | s == S8.pack "http"  = return 80
                        | s == S8.pack "https" = return 443
                        | otherwise = throwIO . userError $
                                        "Unrecognized scheme" ++ (S8.unpack s)

-- | Catch 'IOException's
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch
