
module Main (main) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent
import           Control.Exception (finally)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Socket as Net
import qualified System.IO as IO
import           Text.XHtml.Strict hiding (p)

import           Data.IterIO
import           Data.IterIO.Http
import qualified Data.ListLike as LL

type L = L.ByteString

port :: Net.PortNumber
port = 8000

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

page :: String -> Html -> Html
page pageTitle contents =
  thehtml << [ header << thetitle << pageTitle
             , body << contents
             ]
      
req2Html :: (MonadIO m) => Inum L [Html] m a
req2Html = mkInumM $ do
  req <- lift httpreqI
  liftIO $ when (reqPath req == S8.pack "/slow") $ threadDelay $ 5 * 1000 * 1000
  ifeed [page "Request" $ req2Html' req]

req2Html' :: HttpReq -> Html
req2Html' r = paragraph << (query +++ headers +++ cookies +++ contents)
 where
  query = toHtml [ toHtml $ S8.unpack (reqMethod r) ++ " "
                 , strong <<
                     (S8.unpack (reqHost r)
                      ++ (maybe "" (\p -> ":" ++ show p) $ reqPort r)
                      ++ S8.unpack (reqPath r)
                      ++ (if S8.null q
                            then ""
                            else "?" ++ S8.unpack q))
                 , toHtml $ " HTTP/" ++ show major ++ "." ++ show minor
                 ]
  (major,minor) = reqVers r
  q = reqQuery r
  headers = defs2Html $ reqHeaders r
  cookies = defs2Html $ reqCookies r
  contents = maybe noHtml (toHtml . showContentType) $ reqContentType r
  showContentType (typ,_) = toHtml $ S8.unpack typ
  def2Html (h,v) = toHtml [ strong << (S8.unpack h ++ ": ")
                          , toHtml $ S8.unpack v ]
  defs2Html dd = if null dd
                 then noHtml
                 else ulist << (map ((li <<) . def2Html) dd)

html2L :: (Monad m) => Inum [Html] L m a
html2L = mkInum $ do
  h <- headLI
  return $ L.append (headersL xhtmlHeaders)
                    (U.fromString $ showHtml h)

xhtmlHeaders :: [String]
xhtmlHeaders = ["HTTP/1.1 200 OK", "Content-type: text/html"]

headersL :: [String] -> L
headersL hh = L.append (L.concat (map ((flip L.append crlf) . L.pack) hh)) crlf
 where crlf = L.pack ['\r', '\n']

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
        -- iter' <- lift $ inumMC passCtl $ feedI iter $ chunk buf
        _ <- ifeed buf
        loop iter'

stderrLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e, Eq t, Enum e, Eq e) =>
             t -> Inum t t m a
stderrLog prefix = inumTee $ handleLogI IO.stderr prefix

-- server prototype

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
  enumHandle' h |. stderrLog (L.pack "< ")
     |$ req2Html .| html2L .| handleI h

warn :: String -> IO ()
warn msg = IO.hPutStrLn IO.stderr msg

