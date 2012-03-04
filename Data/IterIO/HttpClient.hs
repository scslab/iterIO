module Data.IterIO.HttpClient ( -- * Simple interface
                                simpleHttp
                              , simpleGetHttp
                              , simpleHeadHttp
                              , genSimpleHttp 
                              -- * Advanced/internal interface
                              , HttpClient(..)
                              , mkHttpClient
                              , httpConnect
                              ) where

import Prelude hiding (catch, head, id, div)

import Control.Monad (when)
import Control.Exception
import Control.Monad.Trans

import Data.Maybe ( isNothing, fromJust)
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
    , hcIsHttps  :: !Bool
    }

-- | Maximum number of redirects (to avoid cycle).
maxNrRedirects :: Int
maxNrRedirects = 5

-- | Given an HTTP client configuration, make the actual connection to
-- server.
httpConnect :: HttpClient -> IO (Iter L IO (), Onum L IO a)
httpConnect hc = do
  let s       = hcSock hc
      isHttps = hcIsHttps hc
  when (isHttps && isNothing (hcSslCtx hc)) $
    throwIO (userError "Need SSL context for  HTTPS")
  Net.connect s (hcSockAddr hc)
  if hcIsHttps hc
    then mkSecure s (fromJust $ hcSslCtx hc)
    else mkInsecure s
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
mkHttpClient :: S                     -- ^ Host
              -> Int                  -- ^ Port
              -> Maybe SSL.SSLContext -- ^ SSL context
              -> Bool                 -- ^ Is HTTPS
              -> IO HttpClient
mkHttpClient host port ctx isHttps = withSocket $ \s -> do
    Net.setSocketOption s Net.KeepAlive 1
    hostA <- getHostAddr (S8.unpack host)
    let a = Net.SockAddrInet (toEnum port) hostA
    return HttpClient { hcSock     = s
                      , hcSockAddr = a
                      , hcSslCtx   = ctx
                      , hcIsHttps  = isHttps }
 where withSocket action = do
         s <- Net.socket Net.AF_INET Net.Stream 6
         catchIO (action s) (\e -> Net.sClose s >> ioError e)
       getHostAddr h = 
         catchIO (Net.inet_addr h) $ \_ -> do
           h' <- getHostByName_safe h
           case Net.hostAddresses h' of
             []     -> err $ "No addresses in host entry for " ++ show h
             (ha:_) -> return ha
       getHostByName_safe h = 
         catchIO (Net.getHostByName h) $ \_ ->
           err $ "Failed to lookup " ++ show h
       err = throwIO . userError

-- | Alias for 'simpleGetHttp'.
simpleHttp :: String                -- ^ URL
           -> Maybe SSL.SSLContext  -- ^ SSL Context
           -> IO (HttpResp IO)
simpleHttp = simpleGetHttp

-- | Given a URL and SSL context, perform a simple GET requrest.
-- Use 'enumHttpResp' to retrieve the body, etc.
simpleGetHttp :: String                -- ^ URL
              -> Maybe SSL.SSLContext  -- ^ SSL Context
              -> IO (HttpResp IO)
simpleGetHttp urlString ctx = do
  req <- getRequest (L8.pack urlString)
  genSimpleHttp req ctx maxNrRedirects True

-- | Given a URL and SSL context, perform a simple HEAD requrest.
-- Use 'enumHttpResp' to retrieve the body, etc.
simpleHeadHttp :: String                -- ^ URL
               -> Maybe SSL.SSLContext  -- ^ SSL Context
               -> IO (HttpResp IO)
simpleHeadHttp urlString ctx = do
  req <- headRequest (L8.pack urlString)
  genSimpleHttp req ctx maxNrRedirects True

-- | Make a general HTTP request.
-- If the request is over HTTPS, the SSL context must be provided.
-- The redirect count is used to limit the number of redirects
-- followed (when receiving a 3xx response); use 0 to return the 
-- direct response. The @passCookies@ flag is used to guard cookies
-- on redirects: because @genSimpleHttp@ performs a \"single request\"
-- it does not parse \"Set-Cookie\" headers and so is unaware of the
-- cookie domain. Hence, the flag is used for the decision in passing
-- the cookie to the location of a redirect.
genSimpleHttp :: HttpReq ()            -- ^ Reqeuest
              -> Maybe SSL.SSLContext  -- ^ SSL Context
              -> Int                   -- ^ Redirect count
              -> Bool                  -- ^ Pass cookies
              -> IO (HttpResp IO)
genSimpleHttp req ctx redirectCount passCookies = do
  let scheme = reqScheme req
      isHttps = scheme == (S8.pack "https")
  port <- maybe (defaultPort scheme) return $ reqPort req
  client <- mkHttpClient (reqHost req) port ctx isHttps
  (sIter,sOnum) <- httpConnect client
  enumHttpReq req |$ sIter  
  resp <- sOnum |$ httpRespI
  if redirectCount > 0
    then handleRedirect req ctx redirectCount resp passCookies
    else return resp
    where defaultPort s | s == S8.pack "http"  = return 80
                        | s == S8.pack "https" = return 443
                        | otherwise = throwIO . userError $
                                        "Unrecognized scheme" ++ S8.unpack s

-- | Given a 3xx response and original request, handle the redirect.
-- The paramets of @handleRedirect@ are the same as 'genSimpleHttp',
-- hwere they are explained. Currently, only reponses with status
-- codes 30[1237] and set \"Location\" header are handled.
handleRedirect :: HttpReq ()           -- ^ Original request
               -> Maybe SSL.SSLContext -- ^ SSL context
               -> Int                  -- ^ Redirect count
               -> HttpResp IO          -- ^ Response
               -> Bool                 -- ^ Pass cookies
               -> IO (HttpResp IO)    
handleRedirect req ctx n resp passCookies = 
  if (respStatus resp `notElem` s300s) || (reqMethod req `notElem` meths)
    then return resp
    else let newLoc = lookup (S8.pack "location") $ respHeaders resp
         in maybe (return resp) doRedirect newLoc
    where s300s = [stat301, stat302, stat303, stat307]
          meths = [S8.pack "GET", S8.pack "HEAD"]
          doRedirect url = do
            newReq <- mkRequestToAbsUri (lazyfy url) (reqMethod req)
            let mReq = newReq { reqHeaders          = reqHeaders req
                              , reqCookies          = if passCookies
                                                        then reqCookies req
                                                        else []
                              , reqContentType      = reqContentType req
                              , reqContentLength    = reqContentLength req
                              , reqTransferEncoding = reqTransferEncoding req
                              , reqIfModifiedSince  = reqIfModifiedSince req
                              , reqSession          = reqSession req }
            genSimpleHttp mReq ctx (n-1) passCookies
  

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

lazyfy :: S -> L
lazyfy = L.pack . S.unpack
