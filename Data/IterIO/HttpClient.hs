module Data.IterIO.HttpClient ( -- * Simple interface
                                simpleHttp, genSimpleHttp 
                              , headRequest, getRequest, postRequest
                                -- * GET, HEAD wrappers
                              , simpleGetHttp, simpleGetHttps
                              , simpleHeadHttp, simpleHeadHttps
                              -- * Advanced interface
                              , HttpClient(..)
                              , mkHttpClient
                              , httpConnect
                              -- * Internal
                              , userAgent
                              , mkRequestToAbsUri
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

import Data.Version (showVersion)
import Paths_iterIO (version)

type L = L.ByteString
type S = S.ByteString


lazyfy :: S -> L
lazyfy = L.pack . S.unpack

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

-- | User agent corresponding to this library.
userAgent :: String
userAgent = "haskell-iterIO/"  ++ showVersion version

-- | An HTTP client.
data HttpClient = HttpClient {
      hcSock     :: !Net.Socket
    -- ^ Socket
    , hcSockAddr :: !Net.SockAddr
    -- ^ Socket address
    , hcSslCtx   :: !(Maybe SSL.SSLContext)
    -- ^ SSL context
    , hcIsHttps  :: !Bool
    -- ^ Use SSL
    }

-- | Maximum number of redirects (to avoid cycle).
maxNrRedirects :: Int
maxNrRedirects = 5

-- | Given an HTTP client configuration, make the actual connection to
-- server.
httpConnect :: MonadIO m => HttpClient -> IO (Iter L m (), Onum L m a)
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


-- | Given the host, port, context, and \"is-https\" flag, create
-- a client value. The returned value can be used with 'httpConnect'
-- to get raw pipes to/from the server.
--
-- /Note:/ Some of this code is from the "HTTP" package.
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

--
-- Simple interface wrappers
--

-- | Perform a simple HTTP GET request. No SSL support.
simpleGetHttp :: MonadIO m
              => String          -- ^ URL
              -> m (HttpResp m)
simpleGetHttp url = simpleHttp (getRequest url) L.empty Nothing

-- | Perform a simple HTTPS GET request.
simpleGetHttps :: MonadIO m
               => String          -- ^ URL
               -> SSL.SSLContext  -- ^ SSL Context
               -> m (HttpResp m)
simpleGetHttps url ctx = simpleHttp (getRequest url) L.empty (Just ctx)

-- | Perform a simple HTTP HEAD request. No SSL support.
simpleHeadHttp :: MonadIO m
               => String          -- ^ URL
               -> m (HttpResp m)
simpleHeadHttp url = simpleHttp (headRequest url) L.empty Nothing

-- | Perform a simple HTTPS HEAD request.
simpleHeadHttps :: MonadIO m
                => String          -- ^ URL
                -> SSL.SSLContext  -- ^ SSL Context
                -> m (HttpResp m)
simpleHeadHttps url ctx = simpleHttp (headRequest url) L.empty (Just ctx)


--
-- Simple interface
--

-- | Perform a simple HTTP request, given the the request header, body
-- and SSL context, if any.
simpleHttp :: MonadIO m
           => HttpReq ()           -- ^ Request header
           -> L                    -- ^ Request body
           -> Maybe SSL.SSLContext -- ^ SSL Context
           -> m (HttpResp m)
simpleHttp req body ctx = genSimpleHttp req body ctx maxNrRedirects True

-- | Make a general HTTP request to host specified in the request.
-- If the request is over HTTPS, the SSL context must be provided.
-- The redirect count is used to limit the number of redirects
-- followed (when receiving a 3xx response); use 0 to return the 
-- direct response. The @passCookies@ flag is used to guard cookies
-- on redirects: because @genSimpleHttp@ performs a \"single request\"
-- it does not parse \"Set-Cookie\" headers and so is unaware of the
-- cookie domain. Hence, the flag is used for the decision in passing
-- the cookie to the location of a redirect.
genSimpleHttp :: MonadIO m
              => HttpReq ()            -- ^ Request header
              -> L                     -- ^ Message body
              -> Maybe SSL.SSLContext  -- ^ SSL Context
              -> Int                   -- ^ Redirect count
              -> Bool                  -- ^ Pass cookies
              -> m (HttpResp m)
genSimpleHttp req body ctx redirectCount passCookies = do
  let scheme = reqScheme req
      isHttps = scheme == (S8.pack "https")
  port <- maybe (defaultPort scheme) return $ reqPort req
  client <- liftIO $ mkHttpClient (reqHost req) port ctx isHttps
  (sIter,sOnum) <- liftIO $ httpConnect client
  enumHttpReq req body |$ sIter  
  resp <- sOnum |$ httpRespI
  if redirectCount > 0
    then handleRedirect req body ctx redirectCount resp passCookies
    else return resp
    where defaultPort s | s == S8.pack "http"  = return 80
                        | s == S8.pack "https" = return 443
                        | otherwise = liftIO . throwIO . userError $
                                        "Unrecognized scheme" ++ S8.unpack s

-- | Given a 3xx response and original request, handle the redirect.
-- The paramets of @handleRedirect@ are the same as 'genSimpleHttp',
-- hwere they are explained. Currently, only reponses with status
-- codes 30[1237] and set \"Location\" header are handled.
handleRedirect :: MonadIO m 
               => HttpReq ()           -- ^ Original request
               -> L                    -- ^ Message body
               -> Maybe SSL.SSLContext -- ^ SSL context
               -> Int                  -- ^ Redirect count
               -> HttpResp m           -- ^ Response
               -> Bool                 -- ^ Pass cookies
               -> m (HttpResp m)    
handleRedirect req body ctx n resp passCookies = 
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
            genSimpleHttp mReq body ctx (n-1) passCookies
  

--
-- Create requests
--

-- | Create a simple HEAD request.
-- The @url@ must be an @absoluteURI@.
headRequest :: String -> HttpReq ()
headRequest url = fromJust $ mkRequestToAbsUri (L8.pack url) $ S8.pack "HEAD"

-- | Create a simple GET request.
-- The @url@ must be an @absoluteURI@.
getRequest :: String -> HttpReq ()
getRequest url = fromJust $ mkRequestToAbsUri (L8.pack url) $ S8.pack "GET"

-- | Given a URL, Content-Type, and message body, perform a simple
-- POST request. Note: message body must be properly encoded (e.g.,
-- URL-encoded if the Content-Type is
-- \"application\/x-www-form-urlencoded\").
postRequest :: String  -- ^ URL
            -> String  -- ^ Content-Type header
            -> L       -- ^ Message body
            -> HttpReq ()
postRequest url ct body =
  let req = fromJust $ mkRequestToAbsUri (L8.pack url) $ S8.pack "POST"
      ctype = (S8.pack "Content-Type", S8.pack ct)
      len   = (S8.pack "Content-Length", S8.pack . show . L8.length $ body)
  in req { reqHeaders = reqHeaders req ++ [ctype, len] }

-- | Createa generic HTTP request, given an absoluteURI:
-- If the URI is not absolute, the parser will fail.
mkRequestToAbsUri :: Monad m => L -> S -> m (HttpReq ())
mkRequestToAbsUri urlString method = do
  (scheme, host, mport, path, query) <- enumPure urlString |$ absUri
  return defaultHttpReq { reqScheme  = scheme
                        , reqMethod  = method
                        , reqPath    = path
                        , reqPathLst = path2list path
                        , reqQuery   = query
                        , reqHost    = host
                        , reqPort    = mport
                        , reqVers    = (1,1)
                        , reqHeaders = [hostHeader host, uaHeader]
                        }
     where uaHeader = (S8.pack "User-Agent", S8.pack userAgent)
           hostHeader host = (S8.pack "Host", host)
