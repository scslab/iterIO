import           Control.Concurrent
import           Control.Exception (finally)
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified OpenSSL as SSL
import qualified Network.Socket as Net
import           Network.Socket (SockAddr)
import           System.Environment (getArgs)
import           System.IO

import           Data.IterIO
import           Data.IterIO.Http
import           HttpServer

type L = L.ByteString

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
              [portS] -> fromInteger $ read portS
              [] -> 8000
              _ -> error "bad args"

  -- create a channel to accept log messages
  logChan <- newChan

  -- process log messages in a separate thread
  _ <- forkIO $ forever $ do
          line <- readChan logChan
          hPutStrLn stderr line

  -- information about the application
  let app = defaultApp { appLog = Just logChan }

  -- handle HTTP connections
  Net.withSocketsDo $ SSL.withOpenSSL $ do
    server <- mkHttpServer port Nothing
    runUntilFinished $ runHttpServer server handleHttpReq (runConn app)

--
-- Request-handling environment
--

data App = App { appLog :: Maybe (Chan String) }

defaultApp :: App
defaultApp = App Nothing

data Conn = Conn { connAddr :: SockAddr
                 , connApp :: App
                 , connRequests :: Int
                 }
type ConnM = ReaderT Conn IO
type ConnI = Iter L ConnM

runConn :: App -> SockAddr -> ConnM a -> IO a
runConn app addr m = runReaderT m $ Conn { connAddr = addr
                                         , connApp = app
                                         , connRequests = 0
                                         }

--
-- Request-handling helpers
--

getPeerAddr :: ConnI SockAddr
getPeerAddr = lift $ asks connAddr

warn :: String -> ConnI ()
warn msg = do
  chM <- lift $ asks (appLog . connApp)
  case chM of
    Nothing -> return ()
    Just ch -> liftIO $ writeChan ch msg

--
-- Request handling
--

handleHttpReq :: HttpReq s -> Iter L ConnM (HttpResp ConnM)
handleHttpReq httpReq = do
  addr <- getPeerAddr
  resp <- return $ resp404 httpReq
  warn $ showReqLine addr httpReq resp
  return resp

--
-- Utilities
--

runUntilFinished :: IO () -> IO ()
runUntilFinished m = do
  sem <- newQSem 0
  _ <- forkIO $ m `finally` signalQSem sem
  waitQSem sem

showReqLine :: (Monad m) => SockAddr -> HttpReq s -> HttpResp m -> String
showReqLine addr req resp =
  show addr
  ++ " " ++ S.unpack (reqMethod req)
  ++ " " ++ S.unpack (reqPath req)
  ++ " -> " ++ showStatus (respStatus resp)

showStatus :: HttpStatus -> String
showStatus (HttpStatus code desc) = show code ++ " " ++ S.unpack desc

