module Message 
  ( httpResponse
  , statusOK, statusSeeOther, statusBadRequest, statusNotFound
  , xhtmlResponse
  , withParm, mapForm
  , urlencoded, multipart
  , headersL
  )
where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.IterIO
import           Data.IterIO.Http
-- import           Data.IterIO.Zlib
import           Text.XHtml.Strict (Html, showHtml)

type L = L.ByteString
type S = S.ByteString


--
-- Response construction
--

httpVersion :: S
httpVersion = S.pack "HTTP/1.1"

statusOK :: S
statusOK = S.pack "200 OK"

statusSeeOther :: S
statusSeeOther = S.pack "303 See Other"

statusBadRequest :: S
statusBadRequest = S.pack "400 Bad Request"

statusNotFound :: S
statusNotFound = S.pack "404 Not Found"

httpResponse :: S -> L -> L
httpResponse status message =
  L.append (L.fromChunks [httpVersion, S.pack " ", status, S.pack "\r\n"]) message

xhtmlResponse :: S -> [String] -> Html -> L
xhtmlResponse status headers h =
  httpResponse status $ L.append (headersL $ xhtmlHeaders ++ headers)
                                 (U.fromString $ showHtml h)

xhtmlHeaders :: [String]
xhtmlHeaders = ["Content-Type: text/html"]

headersL :: [String] -> L
headersL hh = L.append (L.concat (map ((flip L.append crlf) . L.pack) hh)) crlf
 where crlf = L.pack ['\r', '\n']


--
-- Request handling
--

urlencoded :: S
urlencoded = S.pack "application/x-www-form-urlencoded"

multipart :: S
multipart = S.pack "multipart/form-data"

withParm :: (MonadIO m) => String -> HttpReq -> Iter L m a -> Iter L m (Maybe a)
withParm pName req iter = foldForm req handlePart Nothing
 where
  handlePart result part =
    if mpName part == S.pack pName
      then Just <$> iter
      else nullI >> return result

mapForm :: (Monad m) => HttpReq -> (Multipart -> Iter L m a) -> Iter L m ()
mapForm req f = foldForm req (\_ part -> f part >> return ()) ()

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

