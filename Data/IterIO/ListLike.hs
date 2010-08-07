
-- | This module contains basic iteratees and enumerators for working
-- with strings, 'LL.ListLike' objects, file handles, and datagram
-- sockets.
module Data.IterIO.ListLike
    ( -- * Iteratees
      headLikeI, safeHeadLikeI
    , lineI, safeLineI
    , stringExactI, stringMaxI
    , handleI, sockDgramI
    -- * Outer enumerators
    , enumDgram, enumDgramFrom
    , enumHandle, enumHandle'
    , enumFile, enumFile'
    -- * Inner enumerators
    , inumLog, inumhLog
    )where

import Prelude hiding (null)
import Control.Exception (onException)
import Control.Monad
import Control.Monad.Trans
-- import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Monoid
import Data.Char
-- import Data.Word
import Network.Socket
import System.IO

import qualified Data.ListLike as LL

import Data.IterIO.Base
import Data.IterIO.Extra

echr :: (Enum e) => Char -> e
echr = toEnum . ord


--
-- Iters
--

-- | Like 'headI', but works for any 'LL.ListLike' data type.
headLikeI :: (ChunkData t, LL.ListLike t e, Monad m) =>
             Iter t m e
headLikeI = do
  c <- chunkI
  case c of
    Chunk t False | null t -> headLikeI
    Chunk t True  | null t -> throwEOFI "headLikeI"
    Chunk t eof            -> Done (LL.head t) $ Chunk (LL.tail t) eof

-- | Like 'safeHeadI', but works for any 'LL.ListLike' data type.
safeHeadLikeI :: (ChunkData t, LL.ListLike t e, Monad m) =>
                 Iter t m (Maybe e)
safeHeadLikeI = do
  c <- chunkI
  case c of
    Chunk t False | null t -> safeHeadLikeI
    Chunk t True  | null t -> Done Nothing $ Chunk t True
    Chunk t eof            -> Done (Just $ LL.head t) $ Chunk (LL.tail t) eof

-- | Like 'lineI', but returns 'Nothing' on EOF.
safeLineI :: (Monad m, LL.ListLike t e, Eq t, Enum e, Eq e) =>
             Iter t m (Maybe t)
safeLineI = IterF $ return . doline LL.empty
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
               _             -> IterF $ return . doline acc'
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

-- | Return a string that is at most the number of bytes specified in
-- the first arguments, and at least one byte unless EOF is
-- encountered, in which case the empty string is returned.
stringMaxI :: (ChunkData t, LL.ListLike t e, Monad m) =>
              Int
           -> Iter t m t
stringMaxI maxlen = IterF $ return . dostring
    where
      dostring (Chunk s eof) =
          if null s && maxlen > 0 && not eof
            then stringMaxI maxlen
            else case LL.splitAt maxlen s of
                   (h, t) -> Done h $ Chunk t eof

-- | Return a sring that is exactly len bytes, unless an EOF is
-- encountered in which case a shorter string is returned.
stringExactI :: (ChunkData t, LL.ListLike t e, Monad m) =>
                Int
             -> Iter t m t
stringExactI len | len <= 0  = return mempty
                 | otherwise = accumulate mempty
    where
      accumulate acc = do
        t <- stringMaxI (len - LL.length acc)
        if null t then return acc else
            let acc' = LL.append acc t
            in if LL.length t == len then return acc' else accumulate acc'

-- | Put byte strings to a file handle then write an EOF to it. 
handleI :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
           Handle
        -> Iter t m ()
handleI h = putI (liftIO . LL.hPutStr h) (liftIO $ hShutdown h 1)

--
--
sockDgramI :: (MonadIO m, SendRecvString t) =>
              Socket
           -> Maybe SockAddr
           -> Iter [t] m ()
sockDgramI s mdest = do
  mpkt <- safeHeadI
  case mpkt of
    Nothing  -> return ()
    Just pkt -> liftIO (genSendTo s pkt mdest) >> sockDgramI s mdest

--
-- EnumOs
--

-- | Read datagrams from a socket and feed a list of strings (one for
-- each datagram) into an Iteratee.
enumDgram :: (MonadIO m, SendRecvString t) =>
             Socket
          -> EnumO [t] m a
enumDgram sock = enumO $ iterToCodec $ do
  (msg, r, _) <- liftIO $ genRecvFrom sock 0x10000
  if r < 0 then throwEOFI "enumDgram" else return [msg]


-- | Read datagrams from a socket and feed a list of (Bytestring,
-- SockAddr) pairs (one for each datagram) into an Iteratee.
enumDgramFrom :: (MonadIO m, SendRecvString t) =>
                 Socket
              -> EnumO [(t, SockAddr)] m a
enumDgramFrom sock = enumO $ iterToCodec $ do
  (msg, r, addr) <- liftIO $ genRecvFrom sock 0x10000
  if r < 0 then throwEOFI "enumDgramFrom" else return [(msg, addr)]

-- | Feed data from a file handle into an 'Iter' in Lazy
-- 'L.ByteString' format.
enumHandle' :: (MonadIO m) => Handle -> EnumO L.ByteString m a
enumHandle' = enumHandle

-- | Like 'enumHandle'', but can use any 'LL.ListLikeIO' type for the
-- data instead of just 'L.ByteString'.
enumHandle :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
               Handle
            -> EnumO t m a
enumHandle h = enumO $ iterToCodec $ do
  _ <- liftIO $ hWaitForInput h (-1)
  buf <- liftIO $ LL.hGetNonBlocking h defaultChunkSize
  if null buf then throwEOFI "enumHandle" else return buf

-- | Enumerate the contents of a file as a series of lazy
-- 'L.ByteString's.  Note that the enumerator does not change the mode
-- of the handle before reading from it.  Thus, you will often want to
-- call @'hSetBinaryMode' h 'True'@ before using @enumHandle h@.
enumFile' :: (MonadIO m) => FilePath -> EnumO L.ByteString m a
enumFile' = enumFile

-- | Like 'enumFile'', but can use any 'LL.ListLikeIO' type for the
-- data read from the file.
enumFile :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
             FilePath
          -> EnumO t m a
enumFile path = enumObracket (liftIO $ openBinaryFile path ReadMode)
                (liftIO . hClose) codec
    where
      codec h = CodecF (codec h) `liftM` liftIO (LL.hGet h defaultChunkSize)


--
-- EnumIs
--

-- | This inner enumerator is like 'inumNop' in that it passes
-- unmodified 'Chunk's straight through to an iteratee.  However, it
-- also logs the 'Chunk's to a file (which can optionally be truncated
-- or appended to, based on the second argument).
inumLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
           FilePath             -- ^ Path to log to
        -> Bool                 -- ^ True to truncate file
        -> EnumI t t m a
inumLog path trunc iter = do
  h <- liftIO $ openBinaryFile path (if trunc then WriteMode else AppendMode)
  liftIO $ hSetBuffering h NoBuffering
  inumhLog h iter

-- | Like 'inumLog', but takes a writeable file handle rather than a
-- file name.  Closes the handle when done.
inumhLog :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
            Handle
         -> EnumI t t m a
inumhLog h = enumI $ iterToCodec $ do
               Chunk buf eof <- chunkI
               liftIO $ do
                 unless (null buf) $ LL.hPutStr h buf `onException` hClose h
               when eof $ liftIO $ hClose h
               return buf

