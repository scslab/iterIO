{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module contains basic iteratees and enumerators for working
-- with strings, 'LL.ListLike' objects, file handles, and datagram
-- sockets.
module Data.IterIO.ListLike
    ( -- * Iteratees
      putI, sendI
    , headLI, safeHeadLI
    , headI, safeHeadI
    , lineI, safeLineI
    , takeExactI, takeI
    , handleI, sockDgramI, sockStreamI
    , stdoutI
    -- * Control requests
    , SeekMode(..)
    , SizeC(..), SeekC(..), TellC(..), fileCtl
    , GetSocketC(..), socketCtl
    -- * Onums
    , enumDgram, enumDgramFrom, enumStream
    , enumHandle, enumHandle', enumNonBinHandle
    , enumFile, enumFile'
    , enumStdin
    -- * Inums
    , inumTake, inumTakeExact
    , inumLog, inumhLog, inumStderr
    -- * Functions for Iter-Inum pairs
    , pairFinalizer, iterHandle, iterStream
    ) where

import Prelude hiding (null)
import Control.Concurrent
import Control.Exception (toException)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Char
import Data.Monoid
import Data.Typeable
import Network.Socket
import System.IO

import qualified Data.ListLike as LL

import Data.IterIO.Iter
import Data.IterIO.Inum
import Data.IterIO.Extra


echr :: (Enum e) => Char -> e
echr = toEnum . ord

--
-- Iters
--

-- | An Iteratee that puts data to a consumer function, then calls an
-- eof function.  For instance, @'handleI'@ could be defined as:
--
-- @
-- handleI :: (MonadIO m) => 'Handle' -> 'Iter' 'L.ByteString' m ()
-- handleI h = putI ('liftIO' . 'L.hPut' h) ('liftIO' $ 'hShutdown' h 1)
-- @
putI :: (ChunkData t, Monad m) =>
        (t -> Iter t m a)
     -> Iter t m b
     -> Iter t m ()
putI putfn eoffn = doput `finallyI` eoffn
    where doput = do Chunk t eof <- chunkI
                     unless (null t) $ putfn t >> return ()
                     if eof then return () else doput

-- | Send datagrams using a supplied function.  The datagrams are fed
-- as a list of packets, where each element of the list should be a
-- separate datagram.  For example, to create an 'Iter' from a
-- connected UDP socket:
--
-- @
-- udpI :: ('SendRecvString' s, 'MonadIO' m) => 'Socket' -> 'Iter' s m ()
-- udpI sock = sendI $ 'liftIO' . 'genSend' sock
-- @
sendI :: (Show t, Monad m) =>
         (t -> Iter [t] m a)
      -> Iter [t] m ()
sendI sendfn = do
  dgram <- safeHeadI
  case dgram of
    Just pkt -> sendfn pkt >> sendI sendfn
    Nothing  -> return ()

-- | Return the first element when the Iteratee data type is a list.
headLI :: (Show a, Monad m) => Iter [a] m a
headLI = iterF $ dohead
    where dohead (Chunk (a:as) eof) = Done a $ Chunk as eof
          dohead c = IterFail (toException $ mkIterEOF "headLI") c

-- | Return 'Just' the first element when the Iteratee data type
-- is a list, or 'Nothing' on EOF.
safeHeadLI :: (Show a, Monad m) => Iter [a] m (Maybe a)
safeHeadLI = iterF $ dohead
    where dohead (Chunk (a:as) eof) = Done (Just a) $ Chunk as eof
          dohead _                  = Done Nothing chunkEOF


-- | Like 'headLI', but works for any 'LL.ListLike' data type.
headI :: (ChunkData t, LL.ListLike t e, Monad m) =>
         Iter t m e
headI = iterF $ \c@(Chunk t eof) ->
        if null t then IterFail (toException $ mkIterEOF "headI") c
                  else Done (LL.head t) $ Chunk (LL.tail t) eof

-- | Like 'safeHeadLI', but works for any 'LL.ListLike' data type.
safeHeadI :: (ChunkData t, LL.ListLike t e, Monad m) =>
             Iter t m (Maybe e)
safeHeadI = iterF $ \c@(Chunk t eof) ->
            if null t then Done Nothing c
                      else Done (Just $ LL.head t) $ Chunk (LL.tail t) eof

-- | Like 'lineI', but returns 'Nothing' on EOF.
safeLineI :: (ChunkData t, Monad m, LL.ListLike t e, Eq t, Enum e, Eq e) =>
             Iter t m (Maybe t)
safeLineI = iterF $ doline LL.empty
    where
      cr = LL.singleton $ echr '\r'
      nl = LL.singleton $ echr '\n'
      crnl = LL.append cr nl
      eol c = c == echr '\n' || c == echr '\r'
      doline acc (Chunk t eof) =
          let acc' = LL.append acc t
              (l, r) = LL.break eol acc'
              result = dolr eof l r
          in case result of
               Just (l', r') -> Done (Just l') (Chunk r' eof)
               Nothing | eof -> Done Nothing (Chunk acc' True)
               _             -> IterF $ iterF $ doline acc'
      dolr eof l r
          | LL.isPrefixOf nl r = Just (l, LL.drop (LL.length nl) r)
          | LL.isPrefixOf crnl r = Just (l, LL.drop (LL.length crnl) r)
          | LL.isPrefixOf cr r && (eof || r /= cr) =
              Just (l, LL.drop (LL.length cr) r)
          | otherwise = Nothing

-- | Return a line delimited by \\r, \\n, or \\r\\n.
lineI :: (Monad m, ChunkData t, LL.ListLike t e, Eq t, Enum e, Eq e) =>
         Iter t m t
lineI = do
  mline <- safeLineI
  case mline of
    Nothing -> throwEOFI "lineI"
    Just line -> return line

-- | Return 'LL.ListLike' data that is at most the number of elements
-- specified by the first argument, and at least one element unless
-- EOF is encountered or 0 elements are requested, in which case
-- 'LL.empty' is returned.
takeI :: (ChunkData t, LL.ListLike t e, Monad m) => Int -> Iter t m t
takeI maxlen | maxlen <= 0 = return mempty
             | otherwise   = iterF $ \(Chunk s eof) ->
                             case LL.splitAt maxlen s of
                               (h, t) -> Done h $ Chunk t eof

-- | Return 'LL.ListLike' data that is exactly some number of
-- elements, unless an EOF is encountered in which case fewer may be
-- returned.
takeExactI :: (ChunkData t, LL.ListLike t e, Monad m) => Int -> Iter t m t
takeExactI len | len <= 0  = return mempty
               | otherwise = do
  t <- takeI len
  let tlen = LL.length t
  if tlen >= len then return t else LL.append t `liftM` takeI (len - tlen)

-- | Puts strings (or 'LL.ListLikeIO' data) to a file 'Handle', then
-- writes an EOF to the handle.
--
-- Note that this does not put the handle into binary mode.  To do
-- this, you may need to call @'hSetBinaryMode' h 'True'@ on the
-- handle before using it with @handleI@.  Otherwise, Haskell by
-- default will treat the data as UTF-8.  (On the other hand, if the
-- 'Handle' corresponds to a socket and the socket is being read in
-- another thread, calling 'hSetBinaryMode' can cause deadlock, so in
-- this case it is better to have the thread handling reads call
-- 'hSetBinaryMode'.)
--
-- Also note that Haskell be default buffers data written to
-- 'Handle's.  For may network protocols this is a problem.  Don't
-- forget to call @'hSetBuffering' h 'NoBuffering'@ before passing a
-- handle to 'handleI'.
handleI :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
           Handle
        -> Iter t m ()
handleI h = putI (liftIO . LL.hPutStr h) (liftIO $ hShutdown h 1)

-- | Sends a list of packets to a datagram socket.
sockDgramI :: (MonadIO m, SendRecvString t) =>
              Socket
           -> Maybe SockAddr
           -> Iter [t] m ()
sockDgramI s mdest = loop
    where sendit = case mdest of Nothing   -> liftIO . genSend s
                                 Just dest -> liftIO . flip (genSendTo s) dest
          loop = safeHeadI >>= maybe (return ()) (\str -> sendit str >> loop)

sockStreamI :: (ChunkData t, SendRecvString t, MonadIO m) =>
               Socket -> Iter t m ()
sockStreamI sock = putI (liftIO . genSend sock)
                   (liftIO $ shutdown sock ShutdownSend)

-- | An 'Iter' that uses 'LL.hPutStr' to write all output to 'stdout'.
stdoutI :: (LL.ListLikeIO t e, ChunkData t, MonadIO m) => Iter t m ()
stdoutI = putI (liftIO . LL.hPutStr stdout) (return ())

--
-- Control functions
--

-- | A control command (issued with @'ctlI' SizeC@) requesting the
-- size of the current file being enumerated.
data SizeC = SizeC deriving (Typeable)
instance CtlCmd SizeC Integer

-- | A control command for seeking within a file, when a file is being
-- enumerated.  Flushes the residual input data.
data SeekC = SeekC !SeekMode !Integer deriving (Typeable)
instance CtlCmd SeekC ()

-- | A control command for determining the current offset within a
-- file.  Note that this subtracts the size of the residual input data
-- from the offset in the file.  Thus, it will only be accurate when
-- all left-over input data is from the current file.
data TellC = TellC deriving (Typeable)
instance CtlCmd TellC Integer

-- | A handler function for the 'SizeC', 'SeekC', and 'TellC' control
-- requests.  @fileCtl@ is used internally by 'enumFile' and
-- 'enumHandle', and is exposed for similar enumerators to use.
fileCtl :: (ChunkData t, LL.ListLike t e, MonadIO m) =>
           Handle
        -> CtlHandler (Iter () m) t m a
fileCtl h = (mkFlushCtl $ \(SeekC mode pos) -> liftIO (hSeek h mode pos))
            `consCtl` tryTellC
            `consCtl` (mkCtl $ \SizeC -> liftIO (hFileSize h))
            `consCtl` passCtl id
    where tryTellC TellC n c@(Chunk t _) = do
            offset <- liftIO $ hTell h
            return $ runIter (n $ offset - LL.genericLength t) c

-- | A control request that returns the 'Socket' from an enclosing
-- socket enumerator.
data GetSocketC = GetSocketC deriving (Typeable)
instance CtlCmd GetSocketC Socket

-- | A handler for the 'GetSocketC' control request.
socketCtl :: (ChunkData t, MonadIO m) =>
             Socket -> CtlHandler (Iter () m) t m a
socketCtl s = (mkCtl $ \GetSocketC -> return s)
              `consCtl` passCtl id

--
-- Onums
--

-- | Read datagrams (of up to 64KiB in size) from a socket and feed a
-- list of strings (one for each datagram) into an Iteratee.
enumDgram :: (MonadIO m, SendRecvString t) =>
             Socket
          -> Onum [t] m a
enumDgram sock = mkInumC id (socketCtl sock) $
                 liftIO $ liftM (: []) $ genRecv sock 0x10000

-- | Read datagrams from a socket and feed a list of (Bytestring,
-- SockAddr) pairs (one for each datagram) into an Iteratee.
enumDgramFrom :: (MonadIO m, SendRecvString t) =>
                 Socket
              -> Onum [(t, SockAddr)] m a
enumDgramFrom sock = mkInumC id (socketCtl sock) $
                     liftIO $ liftM (: []) $ genRecvFrom sock 0x10000

-- | Read data from a stream (e.g., TCP) socket.
enumStream :: (MonadIO m, ChunkData t, SendRecvString t) =>
              Socket -> Onum t m a
enumStream sock = mkInumC id (socketCtl sock) $ returnSome =<<
                  liftIO (genRecv sock defaultChunkSize)

-- | A variant of 'enumHandle' type restricted to input in the Lazy
-- 'L.ByteString' format.
enumHandle' :: (MonadIO m) => Handle -> Onum L.ByteString m a
enumHandle' = enumHandle

-- | Puts a handle into binary mode with 'hSetBinaryMode', then
-- enumerates data read from the handle to feed an 'Iter' with any
-- 'LL.ListLikeIO' input type.
enumHandle :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
              Handle
           -> Onum t m a
enumHandle h iter = tryIe (liftIO $ hSetBinaryMode h True) >>= check
    where check (Left e)  = Iter $ InumFail e (IterF iter)
          check (Right _) = enumNonBinHandle h iter

-- | Feeds an 'Iter' with data from a file handle, using any input
-- type in the 'LL.ListLikeIO' class.  Note that @enumNonBinHandle@
-- uses the handle as is, unlike 'enumHandle', and so can be used if
-- you want to read the data in non-binary form.
enumNonBinHandle :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
                    Handle
                 -> Onum t m a
enumNonBinHandle h =
    mkInumC id (fileCtl h) $ returnSome =<<
    liftIO (hWaitForInput h (-1) >> LL.hGetNonBlocking h defaultChunkSize)
-- Note that hGet can block when there is some (but not enough) data
-- available.  Thus, we use hWaitForInput followed by hGetNonBlocking.
-- ByteString introduced the call hGetSome for this purpose, but it is
-- not supported by the ListLike package yet.

-- | Enumerate the contents of a file as a series of lazy
-- 'L.ByteString's.  (This is a type-restricted version of
-- 'enumFile'.)
enumFile' :: (MonadIO m) => FilePath -> Onum L.ByteString m a
enumFile' = enumFile

-- | Enumerate the contents of a file for an 'Iter' taking input in
-- any 'LL.ListLikeIO' type.  Note that the file is opened with
-- 'openBinaryFile' to ensure binary mode.
enumFile :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
            FilePath -> Onum t m a
enumFile path = inumBracket (liftIO $ openBinaryFile path ReadMode)
                (liftIO . hClose) enumNonBinHandle

-- | Enumerate standard input.
enumStdin :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) => Onum t m a
enumStdin = enumHandle stdin

--
-- Inums
--

-- | Feed exactly some number of bytes to an 'Iter'.  Throws an error
-- if that many bytes are not available.
inumTakeExact :: (ChunkData t, LL.ListLike t e, Monad m) => Int -> Inum t t m a
inumTakeExact = mkInumM . loop
    where loop n | n <= 0    = return ()
                 | otherwise = do
            t <- dataI
            let (h, r) = LL.splitAt n t
            ungetI r
            _ <- ifeed1 h       -- Keep feeding even if Done
            loop $ n - LL.length h

-- | Feed some number of list elements (bytes in the case of
-- 'L.ByteString's) to an 'Iter', or feed fewer if an EOF is
-- encountered.
inumTake :: (ChunkData t, LL.ListLike t e, Monad m) => Int -> Inum t t m a
inumTake n = mkInumM $ setAutoEOF True >> ipipe (inumTakeExact n)


-- | This inner enumerator is like 'inumNop' in that it passes
-- unmodified 'Chunk's straight through to an iteratee.  However, it
-- also logs the 'Chunk's to a file (which can optionally be truncated
-- or appended to, based on the second argument).  Does not close the
-- file handle when done.
inumLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
           FilePath             -- ^ Path to log to
        -> Bool                 -- ^ True to truncate file
        -> Inum t t m a
inumLog path trunc = inumBracket openLog (liftIO . hClose) inumhLog
    where openLog = liftIO $ do
            h <- openBinaryFile path (if trunc then WriteMode else AppendMode)
            hSetBuffering h NoBuffering
            return h

-- | Like 'inumLog', but takes a writeable file handle rather than a
-- file name.  Does not close the handle when done.
inumhLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
            Handle -> Inum t t m a
inumhLog h = mkInumP $ do buf <- dataI
                          liftIO $ LL.hPutStr h buf
                          return buf

-- | Log a copy of everything to standard error.  (@inumStderr =
-- 'inumhLog' 'stderr'@)
inumStderr :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
              Inum t t m a
inumStderr = inumhLog stderr

--
-- Iter-Onum pairs
--

-- | Add a finalizer to run when an 'Iter' has received an EOF and an
-- 'Inum' has finished.  This works regardless of the order in which
-- the two events happen.
pairFinalizer :: (ChunkData t, ChunkData t1, MonadIO m) =>
                 Iter t m a
              -> Inum t1 t2 m b
              -> IO ()
              -- ^ Cleanup action
              -> IO (Iter t m a, Inum t1 t2 m b)
              -- ^ Cleanup action will run when these two are both done
pairFinalizer iter inum cleanup = do
  mc <- newMVar False
  let end = modifyMVar mc $ \cleanit ->
            when cleanit cleanup >> return (True, ())
  return (iter `finallyI` liftIO end, inum `inumFinally` liftIO end)

iterHandle :: (LL.ListLikeIO t e, ChunkData t, MonadIO m) =>
              Handle -> IO (Iter t m (), Onum t m a)
iterHandle h = pairFinalizer (handleI h) (enumHandle h) (hClose h)

iterStream :: (SendRecvString t, ChunkData t, MonadIO m) =>
              Socket -> IO (Iter t m (), Onum t m a)
iterStream s = pairFinalizer (sockStreamI s) (enumStream s) (sClose s)
