
module Main (main) where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent
import           Control.Exception (finally)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.List (intersperse)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Socket as Net
import qualified System.IO as IO
import           Text.XHtml.Strict hiding (p)

import           Data.IterIO
import           Data.IterIO.Http
import           Data.IterIO.Zlib
import qualified Data.ListLike as LL

type L = L.ByteString
type S = S.ByteString


port :: Net.PortNumber
port = 8000

--
-- Request handler
--

handleRequest :: (MonadIO m) => HttpReq () -> IO.Handle -> Iter L m ()
handleRequest req h = do
  case S.unpack $ reqMethod req of
    "GET" ->
      case reqPathLst req of
        [] -> echo req
        (x:_) -> case S.unpack x of
                  "query" -> ok $ formPage "GET" Nothing
                  "form" -> ok $ formPage "POST" Nothing
                  "form-urlencoded" -> ok $ formPage "POST" (Just urlencoded)
                  "form-multipart" -> ok $ formPage "POST" (Just multipart)
                  "echo-file" -> ok $ echoFilePage
                  "gzip-file" -> ok $ gzipFilePage
                  "slow" -> do liftIO $ threadDelay $ 5 * 1000 * 1000
                               echo req
                  _ -> echo req
    "POST" ->
      case reqPathLst req of
        (x:_) -> case S.unpack x of
                   "echo-file" -> echoFile req h >> return ()
                   "gzip-file" -> gzipFile req h >> return ()
                   "gzip" -> inumGzipResponse .| handleI h  -- process the input raw, not form-encoded
                   "submit" -> echo req
                   _ -> error "Unrecognized action"
        _ -> echo req
    _ -> error "Unrecognized method"
 where
  ok html = inumPure (html2L html) .| handleI h
  echo req = parmsI req >>= ok . page "Request" . request2Html req


--
-- Services for uploaded files
--

echoFile :: (MonadIO m) => HttpReq () -> IO.Handle -> Iter L m (Maybe ())
echoFile req h = withParm "input" req $ inumEchoResponse .| handleI h

inumEchoResponse :: (MonadIO m) => Inum L L m a
inumEchoResponse = mkInumAutoM $ do
  _ <- ifeed $ headersL echoHeaders
  ipipe inumToChunks

gzipFile :: (MonadIO m) => HttpReq () -> IO.Handle -> Iter L m (Maybe ())
gzipFile req h = withParm "input" req $ inumGzipResponse .| handleI h

inumGzipResponse :: (MonadIO m) => Inum L L m a
inumGzipResponse = mkInumAutoM $ do
  _ <- ifeed $ headersL gzipHeaders
  ipipe (inumGzip |. inumToChunks)


--
-- Form processing
--

type Parms = [(FormField, L, Int)]

parmsI :: (Monad m) => HttpReq () -> Iter L m Parms
parmsI req = foldForm req getPart []
 where
  getPart parts mp = do
    front <- takeI 50
    backLen <- countI
    return ((mp,front,backLen):parts)

withParm :: (MonadIO m) => String -> HttpReq ()
         -> Iter L m a -> Iter L m (Maybe a)
withParm pName req iter = foldForm req handlePart Nothing
 where
  handlePart result part =
    if ffName part == S.pack pName
      then Just <$> iter
      else nullI >> return result

{-
mapForm :: (Monad m) => HttpReq -> (Multipart -> Iter L m a) -> Iter L m ()
mapForm req f = foldForm req () (\_ part -> f part >> return ())
-}

--
-- Html rendering
--

request2Html :: HttpReq () -> Parms -> Html
request2Html req parms = toHtml
  [ header2Html req
  , parms2Html parms
  , thediv << (intersperse (toHtml " | ") $ map toHtml
                [ hotlink "/query" << "GET"
                , hotlink "/form" << "POST (default encoding)"
                , hotlink "/form-urlencoded" << "POST (urlencoded)"
                , hotlink "/form-multipart" << "POST (multipart)"
                , hotlink "/echo-file" << "Echo file"
                , hotlink "/gzip-file" << "Gzip file"
                ])
  ]

parms2Html :: Parms -> Html
parms2Html parms =
  if null parms
    then noHtml
    else thediv << [ h3 << "Parameters"
                   , ulist << (map ((li <<) . parm2Html) parms)
                   ]
 where
  parm2Html (mp,front,backLen) = toHtml
    [ strong << (S.unpack (ffName mp) ++ ": ")
    , thespan << L.unpack front
    , if backLen > 0
        then emphasize << ("... (" ++ show (fromIntegral (L.length front) + backLen) ++ " bytes)")
        else noHtml
    ]

header2Html :: HttpReq () -> Html
header2Html r = toHtml [ requestLine, headers, cookies ]
 where
  requestLine = paragraph <<
     [ toHtml $ S.unpack (reqMethod r) ++ " "
     , strong <<
         (S.unpack (reqHost r)
          ++ (maybe "" (\p -> ":" ++ show p) $ reqPort r)
          ++ S.unpack (reqPath r)
          ++ (if S.null q
                then ""
                else "?" ++ S.unpack q))
     , toHtml $ " HTTP/" ++ show major ++ "." ++ show minor
     ]
  (major,minor) = reqVers r
  q = reqQuery r
  headers = defs2Html "Headers" $ reqHeaders r
  cookies = defs2Html "Cookies" $ reqCookies r
  def2Html (h,v) = toHtml [ strong << (S.unpack h ++ ": ")
                          , toHtml $ S.unpack v ]
  defs2Html hdr dd =
    if null dd
       then noHtml
       else thediv << [ h3 << hdr
                      , ulist << (map ((li <<) . def2Html) dd)
                      ]


formPage :: String -> Maybe S -> Html
formPage meth encM = page t $ toHtml
  [ h1 << t
  , paragraph << if meth == "GET"
                   then "It will be submitted as a query in the HTTP request-line."
                   else maybe "It will be submitted with the browser's default Content-Type (likely urlencoded)."
                              (("It will be submitted with Content-Type: " ++) . S.unpack)
                              encM
  , form ! ([ action "/submit", method meth ]
            ++ maybe [] ((:[]) . enctype . S.unpack) encM) <<
      ([ text "data1"
       , text "data2"
       ]
       ++ (if meth == "POST" && encM == Just multipart
            then [file "file1", file "file2"]
            else [])
       ++ [ paragraph << submit "what" meth ]
       )
  ]
 where
  text n = paragraph << [ strong << (n ++ ": "), textfield n ]
  file n = paragraph << [ strong << (n ++ ": "), afile n ]
  t = "Please complete this form"

echoFilePage :: Html
echoFilePage = page t $ toHtml
  [ h1 << t
  , form ! [ action "/echo-file", method "POST", enctype (S.unpack multipart) ] <<
      [ paragraph << "Please select a file to echo.  The server will send it right back to you."
      , afile "input"
      , submit "what" "Echo"
      ]
  ]
 where
  t = "Echo a file"

gzipFilePage :: Html
gzipFilePage = page t $ toHtml
  [ h1 << t
  , form ! [ action "/gzip-file", method "POST", enctype (S.unpack multipart) ] <<
      [ paragraph << "Please select a file to gzip:"
      , afile "input"
      , submit "what" "Gzip"
      ]
  ]
 where
  t = "Gzip a file"

page :: String -> Html -> Html
page pageTitle contents =
  thehtml << [ header << thetitle << pageTitle
             , body << contents
             ]


--
-- Server
--

data Connection = Connection { cxTid :: ThreadId }
type Connections = MVar (Map ThreadId Connection)
data Event = CxDone ThreadId
           | CxNote ThreadId String
type Events = Chan Event

main :: IO ()
main = Net.withSocketsDo $ do
  listener <- myListen port
  connections <- newMVar Map.empty
  events <- newChan
  _ <- forkIO $ forever $ acceptConnection listener connections events
  forever $ handleEvent connections events

acceptConnection :: Net.Socket -> Connections -> Events -> IO ()
acceptConnection listener connections events = do
  (s, addr) <- Net.accept listener
  c <- spawnConnection s addr events
  modifyMVar_ connections $ return . Map.insert (cxTid c) c

handleEvent :: Connections -> Events -> IO ()
handleEvent connections events = do
  event <- readChan events
  case event of
    CxNote tid msg ->
      warn $ show tid ++ ": " ++ msg
    CxDone tid -> do
      modifyMVar_ connections $ return . Map.delete tid
      warn $ show tid ++ " FINISHED"

spawnConnection :: Net.Socket -> Net.SockAddr -> Events -> IO Connection
spawnConnection s addr events = do
  tid <- forkIO $ do
            tid <- myThreadId
            handleConnection s `finally` writeChan events (CxDone tid)
  writeChan events $
    CxNote tid $ "Handling connection from " ++ show addr
  return $ Connection tid

handleConnection :: Net.Socket -> IO ()
handleConnection s = do
  h <- Net.socketToHandle s IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  enumHandle' h |$ do 
      req <- httpReqI
      inumHttpBody req .| handleRequest req h
  IO.hClose h


---
--- Iteratee helpers
---

countI :: (Monad m, ChunkData t, LL.ListLike t e) =>
          Iter t m Int
countI = more 0
 where
  more n = do
    eof <- atEOFI
    if eof
      then return n
      else do buf <- dataI
              more (n + LL.length buf)

{-
stderrLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e, Eq t, Enum e, Eq e) =>
             t -> Inum t t m a
stderrLog prefix = inumTee $ handleLogI IO.stderr prefix

handleLogI :: (MonadIO m, ChunkData t, LL.ListLikeIO t e, Eq t, Enum e, Eq e) =>
              IO.Handle -> t -> Iter t m ()
handleLogI h prefix = forever $ do
  line <- lineI
  liftIO $ LL.hPutStr h prefix
  liftIO $ LL.hPutStrLn h line

inumTee :: (Monad m, ChunkData t) =>
           Iter t m () -> Inum t t m a
inumTee = mkInumAutoM . loop
    where
      loop iter = do
        buf <- lift dataI
        iter' <- lift $ enumPure buf iter
        _ <- ifeed buf
        loop iter'
-}


--
-- Utilities
--

warn :: String -> IO ()
warn msg = IO.hPutStrLn IO.stderr msg

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

urlencoded :: S
urlencoded = S.pack "application/x-www-form-urlencoded"

multipart :: S
multipart = S.pack "multipart/form-data"

html2L :: Html -> L
html2L h = L.append (headersL xhtmlHeaders)
                    (U.fromString $ showHtml h)

xhtmlHeaders :: [String]
xhtmlHeaders = ["HTTP/1.1 200 OK", "Content-Type: text/html"]

gzipHeaders :: [String]
gzipHeaders = ["HTTP/1.1 200 OK", "Content-Type: application/x-gzip", "Transfer-Encoding: chunked"]

echoHeaders :: [String]
echoHeaders = ["HTTP/1.1 200 OK", "Content-Type: application/octet-stream", "Transfer-Encoding: chunked"]

headersL :: [String] -> L
headersL hh = L.append (L.concat (map ((flip L.append crlf) . L.pack) hh)) crlf
 where crlf = L.pack ['\r', '\n']


-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
