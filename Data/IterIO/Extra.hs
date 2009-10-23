{-# LANGUAGE ForeignFunctionInterface #-}

module Data.IterIO.Extra
    ( -- * Deprecated functions
      feed, feedO, feedI
      -- * Functionality missing from system libraries
    , recvStrFrom, sendStr, hShutdown
    ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Foreign.C
import Foreign.Ptr
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy as L
import Network.Socket
import System.IO

import Data.IterIO.Base

import GHC.IOBase (Handle(..), Handle__(..))

foreign import ccall unsafe "sys/socket.h send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "sys/socket.h shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

--
-- Deprecated
--

-- | Feed pure data directly to an iteratee.
feed :: (Monad m, ChunkData t) => t -> Iter t m a -> m (Iter t m a)
feed t iter = runIter iter $ Chunk t False

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

--
-- Some utility functions for things that are made hard by the Haskell
-- libraries
--

-- | Receive data from a datagram socket into a lazy bytestring.
recvStrFrom :: Socket -> Int -> IO (L.ByteString, Int, SockAddr)
recvStrFrom s len = do
  (str, (r, addr)) <- S.createAndTrim' (max 0 len) callRecv
  return (L.fromChunks [str], r, addr)
    where
      callRecv ptr = do (r, addr) <- recvBufFrom s ptr len
                        return (0, max r 0, (r, addr))

-- | Calls the /send/ system call with the contents of a lazy
-- 'Bytestring'.  This is required for connected datagram sockets, as
-- the 'SendTo' haskell function does not allow for a NULL destination
-- address pointer.
sendStr :: Socket -> L.ByteString -> IO Int
sendStr (MkSocket s _ _ _ _) str = do
  r <- S.unsafeUseAsCStringLen (S.concat $ L.toChunks str) $
       \(p, n) -> c_send s p (fromIntegral n) 0
  return $ fromIntegral r

-- | Flushes a file handle and calls the /shutdown/ system call so as
-- to write an EOF to a socket while still being able to read from it.
hShutdown                            :: Handle -> CInt -> IO Int
hShutdown h@(FileHandle _ m) how     = hFlush h >> hShutdown' m how
hShutdown h@(DuplexHandle _ _ m) how = hFlush h >> hShutdown' m how
hShutdown'       :: MVar Handle__ -> CInt -> IO Int
hShutdown' m how = do withMVar m $ \(Handle__ { haFD = fd }) ->
                          liftM fromIntegral $ c_shutdown (fromIntegral fd) how
  
