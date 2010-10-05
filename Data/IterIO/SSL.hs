module Data.IterIO.SSL where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Concurrent
import Control.Exception (throwIO, ErrorCall(..), finally)
import Control.Monad
import Control.Monad.Trans
import qualified Network.Socket as Net
import qualified OpenSSL.Session as SSL
import System.Cmd
import System.Exit

import Data.IterIO.Base
import Data.IterIO.Inum

-- | Simple OpenSSL 'Onum'.
enumSsl :: (MonadIO m) => SSL.SSL -> Onum L.ByteString m a
enumSsl ssl = mkInumM $ irepeat $
              liftIO (SSL.read ssl defaultChunkSize) >>=
                     ifeed1 . L.fromChunks . (: [])

-- | Simple OpenSSL 'Iter'.  Does a uni-directional SSL shutdown when
-- it receives a 'Chunk' with the EOF bit 'True'.
sslI :: (MonadIO m) => SSL.SSL -> Iter L.ByteString m ()
sslI ssl = loop
    where loop = do
            Chunk t eof <- chunkI
            unless (L.null t) $ liftIO $ SSL.lazyWrite ssl t
            if eof then liftIO $ SSL.shutdown ssl SSL.Unidirectional else loop

-- | Turn a socket into an 'Iter' and 'Onum' that use OpenSSL to read
-- and write from the socket, respectively.  Does an SSL
-- bi-directional shutdown and closes the socket when both a) the enum
-- completes and b) the iter has received an EOF chunk.
--
-- This funciton must only be called from within a call to
-- 'SSL.withOpenSSL'.
sslFromSocket :: (MonadIO m) =>
                 SSL.SSLContext
              -- ^ OpenSSL context
              -> Net.Socket
              -- ^ The socket
              -> Bool
              -- ^ 'True' for server handshake, 'False' for client
              -> IO (Iter L.ByteString m (), Onum L.ByteString m a)
sslFromSocket ctx sock server = do
  mc <- newMVar False
  ssl <- SSL.connection ctx sock
  if server then SSL.accept ssl else SSL.connect ssl
  let end = modifyMVar mc $ \closeit ->
            do when closeit $ SSL.shutdown ssl SSL.Bidirectional
                                `finally` Net.sClose sock
               return (True, ())
      iter = sslI ssl >> liftIO end
      enum = mkInumM $ withCleanup (liftIO end) $ ipipe (enumSsl ssl)
  return (iter, enum)

-- | Simplest possible SSL context, loads cert and unencrypted private
-- key from a single file.
simpleContext :: FilePath -> IO SSL.SSLContext
simpleContext keyfile = do
  ctx <- SSL.context
  SSL.contextSetDefaultCiphers ctx
  SSL.contextSetCertificateFile ctx keyfile
  SSL.contextSetPrivateKeyFile ctx keyfile
  SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer False True
  return ctx

-- | Quick and dirty funciton to generate a self signed certificate
-- for testing and stick it in a file.  E.g.:
--
-- > genSelfSigned "testkey.pem" "localhost"
genSelfSigned :: FilePath       -- ^ Filename in which to output key
              -> String         -- ^ Common Name (usually domain name)
              -> IO ()
genSelfSigned file cn = do
  r <- rawSystem "openssl"
       [ "req", "-x509", "-nodes", "-days", "365", "-subj", "/CN=" ++ cn
       , "-newkey", "rsa:1024", "-keyout", file, "-out", file
       ]
  when (r /= ExitSuccess) $ throwIO $ ErrorCall "openssl failed"
