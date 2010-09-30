module Message 
  ( html2L, headersL
  , withParm, mapForm
  , urlencoded, multipart
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

headersL :: [String] -> L
headersL hh = L.append (L.concat (map ((flip L.append crlf) . L.pack) hh)) crlf
 where crlf = L.pack ['\r', '\n']

html2L :: Html -> L
html2L h = L.append (headersL xhtmlHeaders)
                    (U.fromString $ showHtml h)

xhtmlHeaders :: [String]
xhtmlHeaders = ["HTTP/1.1 200 OK", "Content-Type: text/html"]


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

