
module Data.IterIO.Zlib (ZState, deflateInit2, inflateInit2, zCodec
                        , inumZlib, inumGzip, inumGunzip) where

import Prelude hiding (null)
import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad.State.Strict
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import Foreign
import Foreign.C

import Data.IterIO.Base
import Data.IterIO.ZlibInt

import Debug.Trace

data ZState = ZState { zStream :: (ForeignPtr ZStream)
                     , zOp :: (ZFlush -> IO CInt)
                     , zInChunk :: !(ForeignPtr Word8)
                     , zOutChunk :: !(ForeignPtr Word8)
                     , zOut :: L.ByteString -> L.ByteString
                     }

defaultZState :: ZState
defaultZState = ZState { zStream = error "must allocate zStream"
                       , zOp = error "must define zOp"
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

deflateInit2 :: CInt
             -- ^ Compression level (use 'z_DEFAULT_COMPRESSION' for defualt)
             -> ZMethod
             -- ^ Method (use z_DEFLATED)
             -> CInt
             -- ^ windowBits (e.g., 'max_wbits')
             -> CInt
             -- ^ memLevel (e.g., 'def_mem_level')
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

zPushIn :: [S.ByteString] -> ZM [S.ByteString]
zPushIn s = do
  avail <- zPeek avail_in
  if avail > 0
    then return s
    else do
      let (fp, offset, len) = S.toForeignPtr $ head s
      modify $ \zs -> zs { zInChunk = fp }
      zPokeFP next_in fp offset
      zPoke avail_in $ fromIntegral len
      return $ tail s

zOutLen :: ZM Int
zOutLen = withZFP zOutChunk $ \base ->
          if base == nullPtr
            then return 0
            else do
              lim <- zPeek next_out
              return $ minusPtr lim base

zPopOut :: ZM ()
zPopOut = do
  len <- zOutLen
  when (len > 0) $ do
            ochunk <- liftM (\c -> S.fromForeignPtr c 0 len) $ gets zOutChunk
            out <- liftM (. L.chunk ochunk) $ gets zOut
            modify $ \zs -> zs { zOutChunk = S.nullForeignPtr
                               , zOut = out }

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
    _ | r < 0                   -> do cm <- zPeek msg
                                      m <- if cm == nullPtr
                                           then return "zlib failed"
                                           else liftIO $ peekCString cm
                                      liftIO $ throwIO $ ErrorCall m
    _ | otherwise               -> return r
              
zCodec :: (MonadIO m) => ZState -> Codec L.ByteString m L.ByteString
zCodec zs0 = do
  (Chunk dat eof) <- chunkI
  liftIO $ putTraceMsg $ "EOF is " ++ show eof
  (r, zs) <- liftIO $ runStateT (runz eof $ L.toChunks dat) zs0
  if eof || r == z_STREAM_END
    then return $ CodecE $ zOut zs L.empty
    else return $ CodecF (zCodec zs { zOut = id }) $ zOut zs L.empty
    where
      runz False [] = return z_OK
      runz False s  = do s' <- zPushIn s
                         r <- zExec z_NO_FLUSH
                         if null s' then return r else runz False s'
      runz True []  = finish
      runz True s   = do s' <- zPushIn s
                         if null s'
                           then finish
                           else zExec z_NO_FLUSH >> runz True s'
      finish = do r <- zExec z_FINISH
                  zPopOut
                  modify $ \zs -> zs { zOp = error "zOp called after EOF" }
                  return r
                      

inumZlib :: (MonadIO m) =>
            ZState
         -> EnumI L.ByteString L.ByteString m a
inumZlib zs = enumCI noCtls (zCodec zs)
            
inumGzip :: (MonadIO m) => EnumI L.ByteString L.ByteString m a
inumGzip iter = do
  zs <- liftIO (deflateInit2 z_DEFAULT_COMPRESSION z_DEFLATED
                             max_wbits def_mem_level z_DEFAULT_STRATEGY)
  inumZlib zs iter

inumGunzip :: (MonadIO m) => EnumI L.ByteString L.ByteString m a
inumGunzip iter = do
  zs <- liftIO (inflateInit2 $ 32 + max_wbits)
  inumZlib zs iter

-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
