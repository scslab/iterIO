{-# LANGUAGE FlexibleInstances #-}

module Message 
  ( Message(..)
  , FileMsg(..)
  , mimetype'html, mimetype'javascript, mimetype'json
  , inumMsg
  , statusOK, statusSeeOther, statusBadRequest, statusNotFound
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
import           Text.XHtml.Strict

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

class Message msg where
  msgContentType :: msg -> S
  msgBytes :: msg -> L
  msgInum :: (MonadIO m, ChunkData t) => msg -> Inum t L m a

  msgBytes _ = error "msgBytes unimplemented"
  msgInum msg = enumPure $ msgBytes msg

inumMsg :: (MonadIO m, ChunkData tIn, Message msg) =>
           S -> [S] -> msg -> Inum tIn L m a
inumMsg status headers msg = 
  let responseLine = L.fromChunks [httpVersion, S.pack " ", status, S.pack "\r\n"]
      headers' = (S.append (S.pack "Content-Type: ") (msgContentType msg)) : headers
  in mkInumAutoM $ do
    _ <- ifeed $ L.append responseLine (headersL headers')
    ipipe $ msgInum msg

data FileMsg = FileMsg S FilePath

instance Message FileMsg where
  msgContentType (FileMsg contentType _) = contentType
  msgInum (FileMsg _ filePath) = mkInumAutoM $ irun $ enumFile' filePath

instance Message Html where
  msgContentType _ = mimetype'html
  msgBytes h = U.fromString $ showHtml h

instance Message [Char] where
  msgContentType _ = mimetype'html
  msgBytes s = U.fromString $ showHtml $ thehtml <<
    [ header << thetitle << s
    , body << h1 << s
    ]

mimetype'html :: S
mimetype'html = S.pack "text/html"

mimetype'json :: S
mimetype'json = S.pack "application/json; charset=UTF-8"

mimetype'javascript :: S
mimetype'javascript = S.pack "application/javascript; charset=UTF-8"

headersL :: [S] -> L
headersL hh = L.fromChunks $ (map (flip S.append crlf) hh) ++ [crlf]
 where crlf = S.pack "\r\n"


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

