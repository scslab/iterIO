{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -cpp #-}

-- | This module contains deprecated functions plus a few pieces of
-- functionality that are missing from the standard Haskell libraries.
module Data.IterIO.Extra
    ( -- * Deprecated functions
      chunkerToCodec, feed
      -- * Functionality missing from system libraries
    , SendRecvString(..)
    , hShutdown
    ) where

import Control.Monad
-- import Control.Monad.Trans
import Foreign.C
import Foreign.Ptr
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy as L
import Network.Socket
import System.IO

import Data.IterIO.Base

#if __GLASGOW_HASKELL__ <= 611
import Control.Concurrent.MVar
import GHC.IOBase (Handle(..), Handle__(..))
#else /* __GLASGOW_HASKELL__ >= 612 */
import Data.Typeable
import System.IO.Error
import GHC.IO.FD (FD(..))
import GHC.IO.Handle.Types (Handle__(..))
import GHC.IO.Handle.Internals (wantWritableHandle)
#endif /* __GLASGOW_HASKELL__ >= 612 */

foreign import ccall unsafe "sys/socket.h send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "sys/socket.h shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

--
-- Deprecated
--

-- | Creates a 'Codec' from an 'Iter' @iter@ that returns 'Chunk's.
-- The 'Codec' returned will keep offering to translate more input
-- until @iter@ returns a 'Chunk' with the EOF bit set.
chunkerToCodec :: (ChunkData t, Monad m) => Iter t m (Chunk a) -> Codec t m a
chunkerToCodec iter = do
  Chunk d eof <- iter
  if eof
   then return $ CodecE d
   else return $ CodecF (chunkerToCodec iter) d

-- | Feed pure data directly to an iteratee.
feed :: (Monad m, ChunkData t) => t -> Iter t m a -> m (Iter t m a)
feed t iter = execIter $ runIter iter $ Chunk t False

{-
-- | Feed pure data directly to an iteratee from within a function of
-- type 'EnumO'.  Takes the outer 'EnumO' as an argument so as to
-- invoke it recursively when the iteratee returns 'Cont'.
feedO :: (Monad m, ChunkData t) =>
         EnumO t m a            -- ^ Outer Enumerator (that gets more data)
      -> t                      -- ^ Data to feed to the Iteratee
      -> EnumO t m a            -- ^ Takes an Iter and feeds it the data
feedO enum t iter = do
  result <- lift $ feed t iter
  case result of
    IterF _ -> enum result
    done    -> done

-- | Feed a chunk of the inner data type to the inner iteratee from
-- within an 'EnumI'.  Invokes the 'EnumI' in the first argument
-- recursively when the inner Iteratee returns 'Cont'.
feedI :: (ChunkData tOut, ChunkData tIn, Monad m) =>
         EnumI tOut tIn m a     -- ^ Inner Enumerator
      -> tIn                    -- ^ Transcoded data to feed to inner Iter
      -> EnumI tOut tIn m a     -- ^ Takes the inner Iter and feeds it the data
feedI enum t iter = do
    result <- lift $ feed t iter
    case result of
      IterF _ -> enum result
      _       -> return $ result
-}

--
-- Some utility functions for things that are made hard by the Haskell
-- libraries
--

-- | @SendRecvString@ is the class of string-like objects that can be
-- used with datagram sockets.  The 'genSendTo' method works around a
-- bug in the standard Haskell libraries that makes it hard to use
-- connected datagram sockets.  'genSendTo' accepts 'Nothing' as a
-- destination address and then calls the @send@ (rather than
-- @sendto@) system call.
class SendRecvString t where
    genRecvFrom :: Socket -> Int -> IO (t, Int, SockAddr)
    genSendTo :: Socket -> t -> Maybe SockAddr -> IO Int

instance SendRecvString [Char] where
    genRecvFrom s len = recvFrom s len
    genSendTo s str Nothing = send s str
    genSendTo s str (Just dest) = sendTo s str dest

instance SendRecvString S.ByteString where
    genRecvFrom s len = do
      (str, (r, addr)) <- S.createAndTrim' (max 0 len) callRecv
      return (str, r, addr)
        where
          callRecv ptr = do (r, addr) <- recvBufFrom s ptr len
                            return (0, max r 0, (r, addr))
    genSendTo (MkSocket s _ _ _ _) str Nothing = do
      r <- S.unsafeUseAsCStringLen str $
           \(p, n) -> c_send s p (fromIntegral n) 0
      return $ fromIntegral r
    genSendTo s str (Just dest) = do
      S.unsafeUseAsCStringLen str $ \(p, n) -> sendBufTo s p n dest

instance SendRecvString L.ByteString where
    genRecvFrom s len = do
      (str, r, addr) <- genRecvFrom s len
      return (L.fromChunks [str], r, addr)
    -- XXX should to sendTo in terms of sendmsg to use iovecs
    genSendTo s str mdest = genSendTo s (S.concat $ L.toChunks str) mdest


-- | Flushes a file handle and calls the /shutdown/ system call so as
-- to write an EOF to a socket while still being able to read from it.
-- This is very important when the same file handle is being used to
-- to read data in an 'EnumO' and to write data in an 'Iter'.  Proper
-- protocol functioning may require the 'Iter' to send an EOF over the
-- socket, but the 'EnumO' may still be reading from the socket in a
-- different thread.
hShutdown                            :: Handle -> CInt -> IO Int

#if __GLASGOW_HASKELL__ <= 611
hShutdown h@(FileHandle _ m) how     = hFlush h >> hShutdown' m how
hShutdown h@(DuplexHandle _ _ m) how = hFlush h >> hShutdown' m how
hShutdown'       :: MVar Handle__ -> CInt -> IO Int
hShutdown' m how = do withMVar m $ \(Handle__ { haFD = fd }) ->
                          liftM fromIntegral $ c_shutdown (fromIntegral fd) how

#else /* __GLASGOW_HASKELL__ >= 612 */
hShutdown h how = do
  hFlush h
  wantWritableHandle "hShutdown" h $ \Handle__ {haDevice = dev} ->
      case cast dev of
        Just (FD {fdFD = fd}) -> liftM fromEnum $ c_shutdown fd how
        Nothing -> ioError (ioeSetErrorString
                            (mkIOError illegalOperationErrorType
                             "hShutdown" (Just h) Nothing) 
                            "handle is not a file descriptor")

#endif /* __GLASGOW_HASKELL__ >= 612 */
  
