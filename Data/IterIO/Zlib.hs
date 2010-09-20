
module Data.IterIO.Zlib (-- * Codec and Inum functions
                         ZState, deflateInit2, inflateInit2
                        , inumZState, inumZlib, inumGzip, inumGunzip
                        -- * Constants from zlib.h
                        , max_wbits, max_mem_level, def_mem_level, zlib_version
                        , z_DEFAULT_COMPRESSION
                        , ZStrategy, z_FILTERED, z_HUFFMAN_ONLY, z_RLE
                        , z_FIXED, z_DEFAULT_STRATEGY
                        , ZMethod, z_DEFLATED
                        ) where

import Prelude hiding (null)
import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad.State.Strict
-- import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import Foreign
import Foreign.C

import Data.IterIO.Base
import Data.IterIO.Inum
import Data.IterIO.ZlibInt

-- | State used by 'inumZState', the most generic zlib 'Inum'.
-- Create the state using 'deflateInit2' or 'inflateInit2'.
data ZState = ZState { zStream :: (ForeignPtr ZStream)
                     , zOp :: (ZFlush -> IO CInt)
                     , zFinish :: !ZFlush
                     , zInChunk :: !(ForeignPtr Word8)
                     , zOutChunk :: !(ForeignPtr Word8)
                     , zOut :: L.ByteString -> L.ByteString
                     }

defaultZState :: ZState
defaultZState = ZState { zStream = error "must allocate zStream"
                       , zOp = error "must define zOp"
                       , zFinish = z_FINISH
                       , zInChunk = S.nullForeignPtr
                       , zOutChunk = S.nullForeignPtr 
                       , zOut = id
                       }

newZStream :: (Ptr ZStream -> IO CInt) -> IO (ForeignPtr ZStream)
newZStream initfn = do
  zs <- mallocForeignPtrBytes z_stream_size
  withForeignPtr zs $ \ptr ->
      do _ <- S.memset (castPtr ptr) 0 z_stream_size
         err <- initfn ptr
         when (err /= z_OK) $ throwIO $ ErrorCall "newZStream: init failed"
  return zs

-- | Create a 'ZState' for compression.  See the description of
-- @deflateInit2@ in the zlib.h C header file for a more detailed
-- description of the arguments.  Note in particular that the value of
-- @windowBits@ determines the encapsulation format of the compressed
-- data:
--
--   *   8..15 = zlib format
--
--   *  24..31 = gzip format
--
--   * -8..-15 = means raw zlib format with no header
deflateInit2 :: CInt
             -- ^ Compression level (use 'z_DEFAULT_COMPRESSION' for default)
             -> ZMethod
             -- ^ Method (use 'z_DEFLATED')
             -> CInt
             -- ^ @windowBits@ (e.g., 'max_wbits')
             -> CInt
             -- ^ @memLevel@ (e.g., 'def_mem_level')
             -> ZStrategy
             -- ^ strategy (e.g., 'z_DEFAULT_STRATEGY')
             -> IO ZState
deflateInit2 level method windowBits memLevel strategy = do
  z <- newZStream $ \ptr -> (c_deflateInit2 ptr level method windowBits
                             memLevel strategy zlib_version z_stream_size)
  addForeignPtrFinalizer c_deflateEnd z
  return defaultZState { zStream = z
                       , zOp = \flush -> withForeignPtr z $ \zp ->
                         c_deflate zp flush
                       }

-- | Create a 'Zstate' for uncompression.  See the description of
-- @inflateInit2@ in the zlib.h C header file for a more detailed
-- description of the arguments.  Note in particular that the value of
-- @windowBits@ determines the encapsulation format of the compressed
-- data:
--
--   *   8..15 = zlib format
--
--   *  24..31 = gzip format
--
--   *  40..47 = automatically determine zlib/gzip format
--
--   * -8..-15 = means raw zlib format with no header
inflateInit2 :: CInt
             -- ^ windowBits
             -> IO ZState
inflateInit2 windowBits = do
  z <- newZStream $ \ptr -> (c_inflateInit2 ptr windowBits
                             zlib_version z_stream_size)
  addForeignPtrFinalizer c_inflateEnd z
  return defaultZState { zStream = z
                       , zOp = \flush -> withForeignPtr z $ \zp ->
                         c_inflate zp flush
                       , zFinish = z_NO_FLUSH
                       -- Library documentation makes it sound like
                       -- you don't need Z_FINISH for inflating, and
                       -- it could cause problems if the output buffer
                       -- is not large enough.
                       }

type ZM = StateT ZState IO

withZFP :: (ZState -> ForeignPtr a) -> (Ptr a -> ZM b) -> ZM b
withZFP field k = StateT $ \zs ->
                withForeignPtr (field zs) $ \v -> (runStateT $ k v) zs

zPeek :: (Storable a) => (Ptr ZStream -> Ptr a) -> ZM a
zPeek f = withZFP zStream $ liftIO . peek . f

zPoke :: (Storable a) => (Ptr ZStream -> Ptr a) -> a -> ZM ()
zPoke f a = withZFP zStream $ liftIO . flip poke a . f

zPokeFP :: (Ptr ZStream -> Ptr (Ptr Word8)) -> ForeignPtr Word8 -> Int -> ZM ()
zPokeFP f fp offset = withZFP zStream $ \z ->
                      liftIO $ withForeignPtr fp $ \p ->
                      poke (f z) $ p `plusPtr` offset

zMinusPtr :: (Ptr ZStream -> Ptr (Ptr Word8))
          -> (ZState -> ForeignPtr Word8)
          -> ZM Int
zMinusPtr curf basef = withZFP basef $ \base ->
                       if base == nullPtr
                       then return 0
                       else do
                         cur <- zPeek curf
                         return $ cur `minusPtr` base

zPushIn :: L.ByteString -> ZM L.ByteString
zPushIn s = do
  avail <- zPeek avail_in
  if avail > 0 then return s else pushit s
    where
      pushit (L.Chunk h t) = do
              let (fp, offset, len) = S.toForeignPtr h
              modify $ \zs -> zs { zInChunk = fp }
              zPokeFP next_in fp offset
              zPoke avail_in $ fromIntegral len
              return t
      pushit L.Empty = return L.Empty

zPopIn :: L.ByteString -> ZM L.ByteString
zPopIn s = do
  len <- zPeek avail_in
  if len <= 0
    then return s
    else do
      fptr <- gets zInChunk
      offset <- zMinusPtr next_in zInChunk
      zPoke avail_in 0
      return $ L.chunk (S.fromForeignPtr fptr offset $ fromIntegral len) s

zOutLen :: ZM Int
zOutLen = zMinusPtr next_out zOutChunk

zPopOut :: ZM ()
zPopOut = do
  len <- zOutLen
  when (len > 0) $ do
            ochunk <- liftM (\c -> S.fromForeignPtr c 0 len) $ gets zOutChunk
            out <- liftM (. L.chunk ochunk) $ gets zOut
            modify $ \zs -> zs { zOutChunk = S.nullForeignPtr
                               , zOut = out }
            zPoke avail_out 0

zMkSpace :: ZM ()
zMkSpace = do
  avail <- zPeek avail_out
  when (avail <= 0) $ do
             zPopOut
             nchunk <- liftIO $ S.mallocByteString L.defaultChunkSize
             zPokeFP next_out nchunk 0
             zPoke avail_out $ fromIntegral L.defaultChunkSize
             modify $ \zs -> zs { zOutChunk = nchunk }

zExec :: ZFlush -> ZM CInt
zExec flush = do
  zMkSpace
  op <- gets zOp
  r <- withZFP zInChunk $ \_ -> liftIO $ op flush
  avail <- zPeek avail_out
  case () of
    _ | r == z_OK && avail == 0 -> zExec flush
    _ | r == z_NEED_DICT        -> liftIO $ throwIO $ ErrorCall "zlib NEED_DICT"
    _ | r == z_STREAM_END       -> do zPopOut
                                      return r
    _ | r < 0                   -> do cm <- zPeek msg
                                      m <- if cm == nullPtr
                                           then return $ "zlib failed ("
                                                    ++ show r ++ ")"
                                           else liftIO $ peekCString cm
                                      liftIO $ throwIO $ ErrorCall m
    _ | otherwise               -> return r


inumZState :: (MonadIO m) =>
              ZState
           -> Inum L.ByteString L.ByteString m a
inumZState = mkInumM . loop
    where
      loop zs0 = do
        (Chunk dat eof) <- lift chunkI
        ((r, rest), zs) <- liftIO (runStateT (runz eof dat) zs0)
        iunget rest
        done <- ifeed $ zOut zs L.empty
        unless (done || eof || r == z_STREAM_END) $ loop zs { zOut = id }

      runz False L.Empty = return (z_OK, L.Empty)
      runz eof s0 = do
        s <- zPushIn s0
        flush <- if eof && L.null s then gets zFinish else return z_NO_FLUSH
        r <- zExec flush
        if r == z_STREAM_END || L.null s
          then do s' <- zPopIn s; return (r, s')
          else runz eof s

-- | An 'Inum' that compresses in zlib format.  To uncompress, use
-- 'inumGunzip'.
inumZlib :: (MonadIO m) => Inum L.ByteString L.ByteString m a
inumZlib iter = do
  zs <- liftIO (deflateInit2 z_DEFAULT_COMPRESSION z_DEFLATED max_wbits
                             def_mem_level z_DEFAULT_STRATEGY)
  inumZState zs iter

-- | An 'Inum' that compresses in gzip format.
inumGzip :: (MonadIO m) => Inum L.ByteString L.ByteString m a
inumGzip iter = do
  zs <- liftIO (deflateInit2 z_DEFAULT_COMPRESSION z_DEFLATED (16 + max_wbits)
                             def_mem_level z_DEFAULT_STRATEGY)
  inumZState zs iter

-- | An 'Inum' that uncompresses a data in either the zlib or gzip
-- format.  Note that this only uncompresses one gzip stream.  Thus,
-- if you feed in the concatenation of multiple gzipped files,
-- @inumGunzip@ will stop after the first one.  If this is not what
-- you want, then use @'inumRepeat' inumGunzip@ to decode repeated
-- gzip streams.
inumGunzip :: (MonadIO m) => Inum L.ByteString L.ByteString m a
inumGunzip iter = do
  zs <- liftIO $ inflateInit2 (32 + max_wbits)
  inumZState zs iter

-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
