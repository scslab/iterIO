{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -cpp #-}

-- | This module contains miscellaneous functions plus a few pieces of
-- functionality that are missing from the standard Haskell libraries.
module Data.IterIO.Extra
    ( -- * Miscellaneous
      -- chunkerToCodec
      iterLoop
    , inumSplit
    , fixIterPure
      -- * Functionality missing from system libraries
    , SendRecvString(..)
    , hShutdown
    ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.Monoid
import Foreign.C
import Foreign.Ptr
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy as L
import Network.Socket
import System.IO

import Data.IterIO.Base
import Data.IterIO.Inum

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

{-
-- | Creates a 'Codec' from an 'Iter' that returns 'Chunk's.  The
-- 'Codec' returned will keep offering to translate more input until
-- The 'Iter' returns a 'Chunk' with the EOF bit set.
chunkerToCodec :: (ChunkData t, Monad m) => Iter t m (Chunk a) -> Codec t m a
chunkerToCodec iter = do
  Chunk d eof <- iter
  if eof
   then return $ CodecE d
   else return $ CodecF (chunkerToCodec iter) d
-}

-- | Create a loopback @('Iter', 'Onum')@ pair.  The iteratee and
-- enumerator can be used in different threads.  Any data fed into the
-- 'Iter' will in turn be fed by the 'Onum' into whatever 'Iter' it
-- is given.  This is useful for testing a protocol implementation
-- against itself.
iterLoop :: (MonadIO m, ChunkData t, Show t) =>
            m (Iter t m (), Onum t m a)
iterLoop = do
  -- The loopback is implemented with an MVar (MVar Chunk).  The
  -- enumerator waits on the inner MVar, while the iteratee uses the outer 
  -- MVar to avoid races when appending to the stored chunk.
  mv <- liftIO $ newEmptyMVar >>= newMVar
  return (iterF $ iterf mv, enum mv)
    where
      iterf mv c@(Chunk _ eof) = do
             liftIO $ withMVar mv $ \p ->
                 do mp <- tryTakeMVar p
                    putMVar p $ case mp of
                                  Nothing -> c
                                  Just c' -> mappend c' c
             if eof then Done () chunkEOF else iterF $ iterf mv

      enum mv = mkInumM loop
          where loop = do p <- liftIO $ readMVar mv
                          Chunk t eof <- liftIO $ takeMVar p
                          done <- ifeed t
                          when (not $ eof || done) loop
                  
{-
      enum mv = let codec = do
                      p <- liftIO $ readMVar mv
                      Chunk c eof <- liftIO $ takeMVar p
                      return $ if eof then CodecE c else CodecF codec c
                in mkInum codec
-}

-- | Returns an 'Iter' that always returns itself until a result is
-- produced.  You can fuse @inumSplit@ to an 'Iter' to produce an
-- 'Iter' that can safely be fed (e.g., with 'inumPure') from multiple
-- threads.
inumSplit :: (MonadIO m, ChunkData t) => Inum t t m a
inumSplit iter1 = do
  mv <- liftIO $ newMVar $ iter1
  iterF $ iterf mv
    where
      iterf mv (Chunk t eof) = do
        rold <- liftIO $ takeMVar mv
        rnew <- inumMC passCtl $ feedI rold $ chunk t
        liftIO $ putMVar mv rnew
        case rnew of
          IterF _ | not eof -> iterF $ iterf mv
          _                 -> return rnew

{- fixIterPure allows MonadFix instances, which support
   out-of-order name bindings in a "rec" block, provided your file
   has {-# LANGUAGE RecursiveDo #-} at the top.  A contrived example
   would be:

fixtest :: IO Int
fixtest = inumPure [10] `cat` inumPure [1] |$ fixee
    where
      fixee :: Iter [Int] IO Int
      fixee = rec
        liftIO $ putStrLn "test #1"
        c <- return $ a + b
        liftIO $ putStrLn "test #2"
        a <- headI
        liftIO $ putStrLn "test #3"
        b <- headI
        liftIO $ putStrLn "test #4"
        return c

-- A very convoluted way of computing factorial
fixtest2 :: Int -> IO Int
fixtest2 i = do
  f <- inumPure [2] `cat` inumPure [1] |$ mfix fact
  run $ f i
    where
      fact :: (Int -> Iter [Int] IO Int)
           -> Iter [Int] IO (Int -> Iter [Int] IO Int)
      fact f = do
               ignore <- headI
               liftIO $ putStrLn $ "ignoring " ++ show ignore
               base <- headI
               liftIO $ putStrLn $ "base is " ++ show base
               return $ \n -> if n <=  0
                              then return base
                              else liftM (n *) (f $ n - 1)
-}

-- | This is a fixed point combinator for iteratees over monads that
-- have no side effects.  If you wish to use @rec@ with such a monad,
-- you can define an instance of 'MonadFix' in which
-- @'mfix' = fixIterPure@.  However, be warned that this /only/ works
-- when computations in the monad have no side effects, as
-- @fixIterPure@ will repeatedly re-invoke the function passsed in
-- when more input is required (thereby also repeating side-effects).
-- For cases in which the monad may have side effects, if the monad is
-- in the 'MonadIO' class then there is already an 'mfix' instance
-- defined using 'fixMonadIO'.
fixIterPure :: (ChunkData t, MonadFix m) =>
               (a -> Iter t m a) -> Iter t m a
fixIterPure f = IterM $ mfix ff
    where
      ff ~(Done a _)  = check $ f a
      -- Warning: IterF case re-runs function, repeating side effects
      check (IterF _) = return $ iterF $ \c ->
                        fixIterPure $ \a -> feedI (f a) c
      check (IterM m) = m >>= check
      check iter      = return iter


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
class (Show t) => SendRecvString t where
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
-- to read data in an 'Onum' and to write data in an 'Iter'.  Proper
-- protocol functioning may require the 'Iter' to send an EOF (e.g., a
-- TCP FIN segment), but the 'Onum' may still be reading from the
-- socket in a different thread.
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
  
