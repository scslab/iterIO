{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IterIO.SSL where

import Control.Exception (throwIO, ErrorCall(..), finally, onException)
import Control.Monad
import Control.Monad.Trans
import Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal as L (defaultChunkSize)
import Data.Typeable
import qualified Network.Socket as Net
import qualified OpenSSL.Session as SSL
import System.Cmd
import System.Exit

import Data.IterIO.Iter
import Data.IterIO.Inum
import Data.IterIO.ListLike

-- | A wrapper around the type 'SSL.SSL' to make it an instance of the
-- 'Typeable' class.
newtype SslConnection = SslConnection { unSslConnection :: SSL.SSL }
    deriving (Typeable)

-- | Control request to fetch the 'SSL' object associated with an
-- enumerator.
data SslC = SslC deriving (Typeable)
instance CtlCmd SslC SslConnection

-- | Simple OpenSSL 'Onum'.
enumSsl :: (MonadIO m) => SSL.SSL -> Onum L.ByteString m a
enumSsl ssl = mkInumC id ch codec
    where ch = mkCtl (\SslC -> return $ SslConnection ssl)
               `consCtl` (maybe noCtl socketCtl $ SSL.sslSocket ssl)
          codec = do buf <- liftIO (SSL.read ssl L.defaultChunkSize)
                     if S.null buf
                       then return L.empty
                       else return $ L.fromChunks [buf]

-- | Simple OpenSSL 'Iter'.  Does a uni-directional SSL shutdown when
-- it receives a 'Chunk' with the EOF bit 'True'.
sslI :: (MonadIO m) => SSL.SSL -> Iter L.ByteString m ()
sslI ssl = loop
    where loop = do
            Chunk t eof <- chunkI
            unless (L.null t) $ liftIO $ SSL.lazyWrite ssl t
            if eof then liftIO $ SSL.shutdown ssl SSL.Unidirectional else loop

-- | Turn a socket into an 'Iter' and 'Onum' that use OpenSSL to write
-- to and read from the socket, respectively.  Does an SSL
-- bi-directional shutdown and closes the socket when both a) the enum
-- completes and b) the iter has received an EOF chunk.
--
-- If the SSL handshake fails, then @iterSSL@ closes the socket before
-- throwing an exception.
--
-- This funciton must only be invoked from within a call to
-- @withOpenSSL@.
iterSSL :: (MonadIO m) =>
           SSL.SSLContext
        -- ^ OpenSSL context
        -> Net.Socket
        -- ^ The socket
        -> Bool
        -- ^ 'True' for server handshake, 'False' for client
        -> IO (Iter L.ByteString m (), Onum L.ByteString m a)
iterSSL ctx sock server = do
  ssl <- SSL.connection ctx sock `onException` Net.sClose sock
  (if server then SSL.accept ssl else SSL.connect ssl)
                          `onException` Net.sClose sock
  liftIO $ pairFinalizer (sslI ssl) (enumSsl ssl) $
         SSL.shutdown ssl SSL.Bidirectional `finally` Net.sClose sock

-- | Simplest possible SSL context, loads cert and unencrypted private
-- key from a single file.
simpleContext :: FilePath -> IO SSL.SSLContext
simpleContext keyfile = do
  ctx <- SSL.context
  SSL.contextSetDefaultCiphers ctx
  SSL.contextSetCertificateFile ctx keyfile
  SSL.contextSetPrivateKeyFile ctx keyfile
  -- SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer False True
  SSL.contextSetVerificationMode ctx SSL.VerifyNone
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
