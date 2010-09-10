
module Main (main) where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Network.Socket as Net
import qualified System.IO as IO
import Text.XHtml.Strict hiding (p)

import Data.IterIO
import Data.IterIO.Http
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
      
req2Html :: (Monad m) => Inum L [Html] m a
req2Html = mkInum $ do
  req <- httpreqI
  return $ CodecE [page "Request" $ req2Html' req]

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

  showContentType (typ,_) = toHtml $ S8.unpack typ -- XX

  def2Html (h,v) = toHtml [ strong << (S8.unpack h ++ ": ")
                          , toHtml $ S8.unpack v ]
  defs2Html dd = if null dd
                 then noHtml
                 else ulist << (map ((li <<) . def2Html) dd)

html2L :: (Monad m) => Inum [Html] L m a
html2L = mkInum' $ do
  h <- headLI
  return $ L.append (headersL xhtmlHeaders)
                    (U.fromString $ showHtml h)

xhtmlHeaders :: [String]
xhtmlHeaders = ["HTTP/1.1 200 OK", "Content-type: text/html"]

headersL :: [String] -> L
headersL hh = L.append (L.concat (map ((flip L.append crlf) . L.pack) hh)) crlf

crlf :: L
crlf = L.pack ['\r', '\n']

stderrLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) => Inum t t m a
stderrLog = inumhLog IO.stderr

main :: IO ()
main = Net.withSocketsDo $ do
         sock <- myListen port
         (s, addr) <- Net.accept sock
         print addr
         h <- Net.socketToHandle s IO.ReadWriteMode
         IO.hSetBuffering h IO.NoBuffering
         Net.sClose sock
         enumHandle' h |. stderrLog
            |$ req2Html .| html2L .| handleI h
