
module Data.IterIO.Zlib where

import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad (when, liftM)
import Control.Monad.State.Strict
import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import Foreign
import Foreign.C

import Data.IterIO.Base
import Data.IterIO.ZlibInt

newZStream :: (Ptr ZStream -> IO CInt) -> IO (ForeignPtr ZStream)
newZStream init = do
  zs <- mallocForeignPtrBytes z_stream_size
  withForeignPtr zs $ \ptr ->
      do S.memset (castPtr ptr) 0 z_stream_size
         err <- init ptr
         when (err /= z_OK) $ throwIO $ ErrorCall "newZStream: init failed"
  return zs

deflateInit2 :: CInt -> ZMethod -> CInt -> CInt -> ZStrategy
             -> IO (ForeignPtr ZStream)
deflateInit2 level method windowBits memLevel strategy = do
  z <- newZStream $ \ptr -> (c_deflateInit2 ptr level method windowBits
                             memLevel strategy zlib_version z_stream_size)
  addForeignPtrFinalizer c_deflateEnd z
  return z

inflateInit2 :: CInt -> IO (ForeignPtr ZStream)
inflateInit2 windowBits = do
  z <- newZStream $ \ptr -> (c_inflateInit2 ptr windowBits
                             zlib_version z_stream_size)
  addForeignPtrFinalizer c_inflateEnd z
  return z

data ZState = ZState { zStream :: !(ForeignPtr ZStream)
                     , zOp :: !(Ptr ZStream -> ZFlush -> IO CInt)
                     , zChunk :: !(ForeignPtr Word8)
                     , zOut :: L.ByteString -> L.ByteString
                     }

defaultZState :: ZState
defaultZState = ZState { zStream = error "must allocate zStream"
                       , zOp = error "must define zOp"
                       , zChunk = S.nullForeignPtr 
                       , zOut = id
                       }

type ZMT = StateT ZState 

withZ :: (MonadIO m) =>
         (ZState -> ForeignPtr a) -> (Ptr a -> ZMT IO b) -> ZMT m b
withZ field k = StateT $ \zs ->
                liftIO $ withForeignPtr (field zs) $ \v ->
                    (runStateT $ k v) zs

zPeek :: (MonadIO m, Storable a) => (Ptr ZStream -> Ptr a) -> ZMT m a
zPeek f = withZ zStream $ liftIO . peek . f

zPoke :: (MonadIO m, Storable a) => (Ptr ZStream -> Ptr a) -> a -> ZMT m ()
zPoke f a = withZ zStream $ liftIO . flip poke a . f

zOutLen :: (MonadIO m) => ZMT m Int
zOutLen = withZ zChunk $ \base ->
          if base == nullPtr
            then return 0
            else do
              lim <- zPeek next_out
              return $ minusPtr lim base

zPopOut :: (MonadIO m) => ZMT m ()
zPopOut = do
  len <- zOutLen
  when (len > 0) $ do
            ochunk <- liftM (\c -> S.fromForeignPtr c 0 len) $ gets zChunk
            out <- liftM (. L.chunk ochunk) $ gets zOut
            modify $ \zs -> zs { zChunk = S.nullForeignPtr
                               , zOut = out }

zMkSpace :: (MonadIO m) => ZMT m ()
zMkSpace = do
  avail <- zPeek avail_out
  when (avail <= 0) $ do
             zPopOut
             chunk <- S.mallocByteString L.defaultChunkSize
          

{-
zOutLen :: ZMT m Int
zOutLen = do
  base <- gets zOut

withForeignPtr (zStream zs) $ \z ->
             withForeignPtr (zOut zs) $ \base -> do
              lim <- peek $ next_out z
              return $ lim `minusPtr` base
-}


{-
zPopOut :: IO (ZState, L.ByteString -> L.ByteString)

zCodec :: (MonadIO m) => ZState -> Codec L.ByteString m L.ByteString
zCodec zs = withForeignPtr zs $ \z ->
-}
            
  

-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
