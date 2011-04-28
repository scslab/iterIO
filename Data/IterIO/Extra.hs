{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module contains miscellaneous functions plus a few pieces of
-- functionality that are missing from the standard Haskell libraries.
module Data.IterIO.Extra
    ( -- * Miscellaneous
      iterLoop
    , inumSplit
    -- , fixIterPure
      -- * Functionality missing from system libraries
    , SendRecvString(..)
    , hShutdown
    -- * Debugging functions
    , traceInput, traceI
    ) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.ByteString.Internal (inlinePerformIO)
import Data.Monoid
import Debug.Trace
import Foreign.C
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Network.Socket
import Network.Socket.ByteString as S
import Network.Socket.ByteString.Lazy as L
import System.IO

import Data.IterIO.Iter
import Data.IterIO.Inum

import Data.Typeable
import System.IO.Error
import GHC.IO.FD (FD(..))
import GHC.IO.Handle.Types (Handle__(..))
import GHC.IO.Handle.Internals (wantWritableHandle)

foreign import ccall unsafe "sys/socket.h shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

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
  return (iter mv, enum mv)
    where
      iter mv = do
             c@(Chunk _ eof) <- chunkI
             liftIO $ withMVar mv $ \p ->
                 do mp <- tryTakeMVar p
                    putMVar p $ case mp of
                                  Nothing -> c
                                  Just c' -> mappend c' c
             if eof then return () else iter mv

      -- Note the ifeed mempty, which is there in case the enum feeds
      -- an iter that starts with a liftIO or something, and the other
      -- half of the loopback interface waits for the result of that
      -- liftIO to start producing data.
      enum mv = mkInumM (ifeed mempty >> loop)
          where loop = do p <- liftIO $ readMVar mv
                          Chunk t eof <- liftIO $ takeMVar p
                          done <- ifeed t
                          when (not $ eof || done) loop
                  
-- | Returns an 'Iter' that always returns itself until a result is
-- produced.  You can fuse @inumSplit@ to an 'Iter' to produce an
-- 'Iter' that can safely be fed (e.g., with 'enumPure') from multiple
-- threads.
inumSplit :: (MonadIO m, ChunkData t) => Inum t t m a
inumSplit iter1 = do
  mv <- liftIO $ newMVar $ IterF iter1
  iter mv
    where
      iter mv = do
        (Chunk t eof) <- chunkI
        rold <- liftIO $ takeMVar mv
        rnew <- runIterMC (passCtl pullupResid) (reRunIter rold) $ chunk t
        liftIO $ putMVar mv rnew
        case rnew of
          IterF _ | not eof -> iter mv
          _                 -> return rnew

{- fixIterPure allows MonadFix instances, which support
   out-of-order name bindings in a "rec" block, provided your file
   has {-# LANGUAGE RecursiveDo #-} at the top.  A contrived example
   would be:

fixtest :: IO Int
fixtest = enumPure [10] `cat` enumPure [1] |$ fixee
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
  f <- enumPure [2] `cat` enumPure [1] |$ mfix fact
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
fixIterPure f = Iter $ \c ->
  let ff ~(Done a _)  = check $ runIter (f a) c
      -- Warning: IterF case re-runs function, repeating side effects
      check (IterF _) = return $ IterF $ Iter $ \c' ->
                        runIter (fixIterPure f) (mappend c c')
      check (IterM m) = m >>= check
      check r         = return r
  in IterM $ mfix ff
-}


--
-- Some utility functions for things that are made hard by the Haskell
-- libraries
--

-- | @SendRecvString@ is the class of string-like objects that can be
-- used with datagram sockets.
class (Show t) => SendRecvString t where
    genRecv     :: Socket -> Int -> IO t
    genSend     :: Socket -> t -> IO ()
    genRecvFrom :: Socket -> Int -> IO (t, SockAddr)
    genSendTo   :: Socket -> t -> SockAddr -> IO ()

instance SendRecvString [Char] where
    genRecv s len        = liftM S8.unpack $ S.recv s len
    genSend s str        = S.sendAll s (S8.pack str)
    genRecvFrom s len    = do (str, a) <- S.recvFrom s len
                              return (S8.unpack str, a)
    genSendTo s str dest = S.sendAllTo s (S8.pack str) dest

instance SendRecvString S.ByteString where
    genRecv s len        = S.recv s len
    genSend s str        = S.sendAll s str
    genRecvFrom s len    = S.recvFrom s len
    genSendTo s str dest = S.sendAllTo s str dest

instance SendRecvString L.ByteString where
    genRecv s len        = do str <- S.recv s len
                              return $ L.fromChunks [str]
    genSend s str        = L.sendAll s str
    genRecvFrom s len    = do (str, a) <- S.recvFrom s len
                              return (L.fromChunks [str], a)
    genSendTo s str dest = S.sendManyTo s (L.toChunks str) dest

-- | Flushes a file handle and calls the /shutdown/ system call so as
-- to write an EOF to a socket while still being able to read from it.
-- This is very important when the same file handle is being used to
-- to read data in an 'Onum' and to write data in an 'Iter'.  Proper
-- protocol functioning may require the 'Iter' to send an EOF (e.g., a
-- TCP FIN segment), but the 'Onum' may still be reading from the
-- socket in a different thread.
hShutdown                            :: Handle -> CInt -> IO Int
hShutdown h how = do
  hFlush h
  wantWritableHandle "hShutdown" h $ \Handle__ {haDevice = dev} ->
      case cast dev of
        Just (FD {fdFD = fd}) -> liftM fromEnum $ c_shutdown fd how
        Nothing -> ioError (ioeSetErrorString
                            (mkIOError illegalOperationErrorType
                             "hShutdown" (Just h) Nothing) 
                            "handle is not a file descriptor")
  
--
-- Debugging
--

-- | For debugging, print a tag along with the current residual input.
-- Not referentially transparent.
traceInput :: (ChunkData t, Monad m) => String -> Iter t m ()
traceInput tag = Iter $ \c -> trace (tag ++ ": " ++ show c) $ Done () c

-- | For debugging.  Print the current thread ID and a message.  Not
-- referentially transparent.
traceI :: (ChunkData t, Monad m) => String -> Iter t m ()
traceI msg = Iter $ \c -> inlinePerformIO $ do
               tid <- myThreadId
               putTraceMsg $ show tid ++ ": " ++ msg
               return $ Done () c
