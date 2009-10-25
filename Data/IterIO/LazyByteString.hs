
module Data.IterIO.LazyByteString
    ( -- * Iters
      safeLineI, lineI
    , stringMaxI, stringExactI
    , handleI
    -- * Outer enumerators
    , enumDgram, enumDgramFrom
    , enumHandle, enumFile
    -- * Inner enumerators
    , inumLog, inumhLog
    ) where

import Prelude hiding (null)
import Control.Exception (onException)
import Control.Monad
import Control.Monad.Trans
-- import qualified Data.ByteString as S
-- import qualified Data.ByteString.Internal as S
-- import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy.Internal as L (defaultChunkSize)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int
import Network.Socket
import System.IO

import Data.IterIO.Base
import Data.IterIO.Extra

-- | Turn a 'L.Bytestring' into a 'Chunk'.  Set the EOF marker when
-- the 'L.Bytestring' is empty.
dataToChunk :: (ChunkData t) => t -> Chunk t
dataToChunk t = Chunk t $ null t

--
-- Iters
--

-- | Like 'lineI', but returns 'Nothing' on EOF.
safeLineI :: (Monad m) => Iter L.ByteString m (Maybe L.ByteString)
safeLineI = IterF $ return . doline L.empty
    where
      eol c = c == '\r' || c == '\n'
      doline acc (Chunk t eof) =
          let acc' = L.append acc t
              (l, r) = L8.break eol acc'
          in case () of
               () | eof && L.null r ->
                      Done Nothing (Chunk acc' eof)
                  | not eof && (L.null r || r == L8.singleton '\r') ->
                      IterF $ return . doline acc'
                  | L.take 2 r == L8.pack "\r\n" ->
                      Done (Just l) $ Chunk (L.drop 2 r) eof
                  | otherwise ->
                      Done (Just l) $ Chunk (L.drop 1 r) eof

-- | Return a line delimited by \\r, \\n, or \\r\\n.
lineI :: (Monad m) => Iter L.ByteString m L.ByteString
lineI = do
  mline <- safeLineI
  case mline of
    Nothing -> throwEOFI "lineI"
    Just line -> return line

-- | Return a string that is at most the number of bytes specified in
-- the first arguments, and at least one byte unless EOF is
-- encountered, in which case the empty string is returned.
stringMaxI :: (Monad m) => Int64 -> Iter L.ByteString m L.ByteString
stringMaxI maxlen = IterF $ return . dostring
    where
      dostring (Chunk s eof) =
          if L.null s && maxlen > 0 && not eof
            then stringMaxI maxlen
            else case L.splitAt maxlen s of
                   (h, t) -> Done h $ Chunk t eof

-- | Return a sring that is exactly len bytes, unless an EOF is
-- encountered in which case a shorter string is returned.
stringExactI :: (Monad m) => Int64 -> Iter L.ByteString m L.ByteString
stringExactI len | len <= 0  = return L.empty
                 | otherwise = accumulate L.empty
    where
      accumulate acc = do
        t <- stringMaxI (len - L.length acc)
        if L.null t then return acc else
            let acc' = L.append acc t
            in if L.length t == len then return acc' else accumulate acc'

-- | Put byte strings to a file handle then write an EOF to it. 
handleI :: (MonadIO m) => Handle -> Iter L.ByteString m ()
handleI h = putI (liftIO . L.hPut h) (liftIO $ hShutdown h 1)


--
-- EnumOs
--

-- | Read datagrams from a socket and feed a list of strings (one for
-- each datagram) into an Iteratee.
enumDgram :: (MonadIO m) => Socket
          -> EnumO [L.ByteString] m a
enumDgram sock = enumO $ do
                   (msg, r, _) <- liftIO $ genRecvFrom sock 0x10000
                   return $ if r < 0 then chunkEOF else chunk [msg]

-- | Read datagrams from a socket and feed a list of (Bytestring,
-- SockAddr) pairs (one for each datagram) into an Iteratee.
enumDgramFrom :: (MonadIO m) => Socket
          -> EnumO [(L.ByteString, SockAddr)] m a
enumDgramFrom sock = enumO $ do
  (msg, r, addr) <- liftIO $ genRecvFrom sock 0x10000
  return $ if r < 0 then chunkEOF else chunk [(msg, addr)]

-- | Feed data from a file handle into an Iteratee.
enumHandle :: (MonadIO m) =>
              Handle
           -> EnumO L.ByteString m a
enumHandle h = enumO $ do
                 liftIO $ hWaitForInput h (-1)
                 buf <- liftIO $ L.hGetNonBlocking h L.defaultChunkSize
                 return $ Chunk buf $ L.null buf

enumFile :: (MonadIO m) =>
            FilePath
         -> EnumO L.ByteString m a
enumFile path = enumObracket (liftIO $ openFile path ReadMode) (liftIO . hClose)
                (\h -> liftIO (L.hGet h L.defaultChunkSize)
                       >>= return . dataToChunk)
      
--
-- EnumIs
--

-- | This inner enumerator is like 'inumNop' in that it passes
-- unmodified 'Chunk's straight through to an iteratee.  However, it
-- also logs the 'Chunk's to a file (which can optionally be trucated
-- or appended to, based on the second argument).
inumLog :: (MonadIO m) =>
           FilePath             -- ^ Path to log to
        -> Bool                 -- ^ True to truncate file
        -> EnumI L.ByteString L.ByteString m a
inumLog path trunc iter = do
  h <- liftIO $ openFile path (if trunc then WriteMode else AppendMode)
  liftIO $ hSetBuffering h NoBuffering
  inumhLog h iter

-- | Like 'inumLog', but takes a writeable file handle rather than a
-- file name.
inumhLog :: (MonadIO m) =>
            Handle
         -> EnumI L.ByteString L.ByteString m a
inumhLog h =
    enumI $ do
      c@(Chunk buf eof) <- chunkI
      liftIO $ do
              unless (L.null buf) $ L.hPut h buf `onException` hClose h
              when eof $ hClose h
      return c

