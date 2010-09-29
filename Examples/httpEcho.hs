
module Main (main) where

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
import qualified Data.ListLike as LL

type L = L.ByteString
type S = S.ByteString


port :: Net.PortNumber
port = 8000

--
-- Request handler
--

handleRequest :: (MonadIO m) => IO.Handle -> Iter L m ()
handleRequest h = do
  req <- httpreqI
  case S.unpack $ reqMethod req of
    "GET" ->
      case reqPathLst req of
        [] -> echo req
        (x:_) -> case S.unpack x of
                  "query" -> ok $ formPage "GET" Nothing
                  "form" -> ok $ formPage "POST" Nothing
                  "form-urlencoded" -> ok $ formPage "POST" (Just urlencoded)
                  "form-multipart" -> ok $ formPage "POST" (Just multipart)
                  "slow" -> do liftIO $ threadDelay $ 5 * 1000 * 1000
                               echo req
                  _ -> echo req
    "POST" -> echo req
    _ -> error $ "Unrecognized method"
 where
  ok html = inumPure (html2L html) .| handleI h
  echo req = parmsI req >>= ok . page "Request" . request2Html req

type Parms = [(Multipart, L, Int)]

parmsI :: (Monad m) => HttpReq -> Iter L m Parms
parmsI req = foldParms [] getPart
 where
  getPart parts mp = do
    front <- takeExactI 50
    backLen <- countI
    return ((mp,front,backLen):parts)
  foldParms = case reqContentType req of
                Nothing -> foldQuery req
                _ -> foldForm req


--
-- Html rendering
--

request2Html :: HttpReq -> Parms -> Html
request2Html req parms = toHtml
  [ header2Html req
  , parms2Html parms
  , thediv << (intersperse (toHtml " | ") $ map toHtml
                [ hotlink "/query" << "GET"
                , hotlink "/form" << "POST (default encoding)"
                , hotlink "/form-urlencoded" << "POST (urlencoded)"
                , hotlink "/form-multipart" << "POST (multipart)"
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
    [ strong << (S.unpack (mpName mp) ++ ": ")
    , thespan << L.unpack front
    , if backLen > 0
        then emphasize << ("... (" ++ show (fromIntegral (L.length front) + backLen) ++ " bytes)")
        else noHtml
    ]

header2Html :: HttpReq -> Html
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
  enumHandle' h
     -- |. stderrLog (L.pack "< ")
     -- |$ req2Html .| html2L .| handleI h
     |$ handleRequest h


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
        iter' <- lift $ inumPure buf iter
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
xhtmlHeaders = ["HTTP/1.1 200 OK", "Content-type: text/html"]

headersL :: [String] -> L
headersL hh = L.append (L.concat (map ((flip L.append crlf) . L.pack) hh)) crlf
 where crlf = L.pack ['\r', '\n']

