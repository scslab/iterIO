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
    , handleI, sockDgramI
    , stdoutI
    -- * Control requests
    , SeekMode(..)
    , SizeC(..), SeekC(..), TellC(..)
    , fileCtl
    -- * Onums
    , enumDgram, enumDgramFrom
    , enumHandle, enumHandle', enumNonBinHandle
    , enumFile, enumFile'
    , enumStdin
    -- * Inums
    , inumTake, inumTakeExact
    , inumLog, inumhLog
    ) where

import Prelude hiding (null)
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

import Data.IterIO.Base
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
-- > handleI :: (MonadIO m) => Handle -> Iter L.ByteString m ()
-- > handleI h = putI (liftIO . L.hPut h) (liftIO $ hShutdown h 1)
--
putI :: (ChunkData t, Monad m) =>
        (t -> Iter t m a)
     -> Iter t m b
     -> Iter t m ()
putI putfn eoffn = do
  Chunk t eof <- chunkI
  unless (null t) $ putfn t >> return ()
  if eof then eoffn >> return () else putI putfn eoffn

-- | Send datagrams using a supplied function.  The datagrams are fed
-- as a list of packets, where each element of the list should be a
-- separate datagram.
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
          dohead _                  = throwEOFI "headLI"

-- | Return 'Just' the first element when the Iteratee data type
-- is a list, or 'Nothing' on EOF.
safeHeadLI :: (Show a, Monad m) => Iter [a] m (Maybe a)
safeHeadLI = iterF $ dohead
    where dohead (Chunk (a:as) eof) = Done (Just a) $ Chunk as eof
          dohead _                  = Done Nothing chunkEOF


-- | Like 'headLI', but works for any 'LL.ListLike' data type.
headI :: (ChunkData t, LL.ListLike t e, Monad m) =>
         Iter t m e
headI = do
  (Chunk t eof) <- chunkI
  if null t then throwEOFI "headI"
            else Done (LL.head t) $ Chunk (LL.tail t) eof

-- | Like 'safeHeadLI', but works for any 'LL.ListLike' data type.
safeHeadI :: (ChunkData t, LL.ListLike t e, Monad m) =>
             Iter t m (Maybe e)
safeHeadI = do
  (Chunk t eof) <- chunkI
  if null t then return Nothing
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
               _             -> iterF $ doline acc'
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
-- Also note that Haskell be default buffers data written to handle.
-- For may network protocols this is a problem.  Don't forget to call
-- @'hSetBuffering' h 'NoBuffering'@ before passing a handle to
-- 'handleI'.
handleI :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
           Handle
        -> Iter t m ()
handleI h = putI (liftIO . LL.hPutStr h) (liftIO $ hShutdown h 1)

-- | Sends a list of packets to a datagram socket.
sockDgramI :: (MonadIO m, SendRecvString t) =>
              Socket
           -> Maybe SockAddr
           -> Iter [t] m ()
sockDgramI s mdest = do
  mpkt <- safeHeadI
  case mpkt of
    Nothing  -> return ()
    Just pkt -> liftIO (genSendTo s pkt mdest) >> sockDgramI s mdest

-- | An 'Iter' that uses 'LL.hPutStr' to writes all output to
-- 'stdout'.
stdoutI :: (LL.ListLikeIO t e, ChunkData t, MonadIO m) => Iter t m ()
stdoutI = putI (liftIO . LL.hPutStr stdout) (return ())

--
-- Control functions
--

-- | A control command requesting the size of the current file being
-- enumerated.
data SizeC = SizeC deriving (Typeable)
instance CtlCmd SizeC Integer

-- | A control command for seeking within a file, when a file is being
-- enumerated.
data SeekC = SeekC !SeekMode !Integer deriving (Typeable)
instance CtlCmd SeekC ()

-- | A control command for determining the current offset within a
-- file.  Note that this will only be accurate when there is not
-- left-over input data.
data TellC = TellC deriving (Typeable)
instance CtlCmd TellC Integer

fileCtl :: (ChunkData t, MonadIO m) => Handle -> CtlHandler (Iter t m)
fileCtl h = (\(SeekC mode pos) -> liftIO (hSeek h mode pos))
            `consCtl` (\TellC -> liftIO (hTell h))
            `consCtl` (\SizeC -> liftIO (hFileSize h))
            `consCtl` passCtl

--
-- Onums
--

-- | Read datagrams from a socket and feed a list of strings (one for
-- each datagram) into an Iteratee.
enumDgram :: (MonadIO m, SendRecvString t) =>
             Socket
          -> Onum [t] m a
enumDgram sock = mkInumM $ irepeat $ do
  (msg, r, _) <- liftIO $ genRecvFrom sock 0x10000
  if r < 0 then lift $ throwEOFI "enumDgram" else ifeed [msg]


-- | Read datagrams from a socket and feed a list of (Bytestring,
-- SockAddr) pairs (one for each datagram) into an Iteratee.
enumDgramFrom :: (MonadIO m, SendRecvString t) =>
                 Socket
              -> Onum [(t, SockAddr)] m a
enumDgramFrom sock = mkInumM $ irepeat $ do
  (msg, r, addr) <- liftIO $ genRecvFrom sock 0x10000
  if r < 0 then lift $ throwEOFI "enumDgramFrom" else ifeed [(msg, addr)]

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
enumHandle h iter = do
  liftIO $ hSetBinaryMode h True
  enumNonBinHandle h iter

-- | Feeds an 'Iter' with data from a file handle, using any input
-- type in the 'LL.ListLikeIO' class.  Note that @enumNonBinHandle@
-- uses the handle as is, unlike 'enumHandle', and so can be used if
-- you want to read the data in non-binary form.
enumNonBinHandle :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
                    Handle
                 -> Onum t m a
enumNonBinHandle h = mkInumM $ do
  setCtlHandler (fileCtl h)
  irepeat $ liftIO (hWaitForInput h (-1) >>
                    LL.hGetNonBlocking h defaultChunkSize) >>= ifeed1
-- Note that hGet can block when there is some (but not enough) data
-- available.  Thus, we use hWaitForInput followed by hGetNonBlocking.

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
enumFile path = mkInumM $ do
  h <- liftIO $ openBinaryFile path ReadMode
  addCleanup $ liftIO $ hClose h
  setCtlHandler $ fileCtl h
  irepeat $ liftIO (LL.hGet h defaultChunkSize) >>= ifeed1

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
            t <- lift dataI
            let (h, r) = LL.splitAt n t
            iunget r
            _ <- ifeed1 h       -- Keep feeding even if Done
            loop $ n - LL.length h

-- | Feed some number of bytes to an 'Iter', or feed fewer if an EOF
-- is encountered.
inumTake :: (ChunkData t, LL.ListLike t e, Monad m) => Int -> Inum t t m a
inumTake n = mkInumM $ setAutoEOF True >> ipipe (inumTakeExact n)


-- | This inner enumerator is like 'inumNop' in that it passes
-- unmodified 'Chunk's straight through to an iteratee.  However, it
-- also logs the 'Chunk's to a file (which can optionally be truncated
-- or appended to, based on the second argument).
inumLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
           FilePath             -- ^ Path to log to
        -> Bool                 -- ^ True to truncate file
        -> Inum t t m a
inumLog path trunc = mkInumM $ do
  h <- liftIO $ openBinaryFile path (if trunc then WriteMode else AppendMode)
  liftIO $ hSetBuffering h NoBuffering
  addCleanup (liftIO $ hClose h)
  ipipe $ inumhLog h

-- | Like 'inumLog', but takes a writeable file handle rather than a
-- file name.  Does not close the handle when done.
inumhLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
            Handle -> Inum t t m a
inumhLog h = mkInumM $ do addCleanup (ipopresid >>= iunget)
                          irepeat $ do buf <- lift dataI
                                       liftIO $ LL.hPutStr h buf
                                       ifeed buf
