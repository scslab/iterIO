
module Arc4 ( Arc4, a4new
            , a4byte , a4chunk, a4stringN
            , Arc4IO
            , a42io, io2a4
            , a4byteIO, a4chunkIO
            , A4Random
            , a4RandomNew
            , a4RandomBits, a4RandomBool
            , a4RandomChunkN, a4RandomStringN
            ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
-- import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import Data.Array.IO.Internals
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.IORef
import Data.Word
import Foreign.Storable
-- import System.IO
import System.IO.Unsafe
import System.Time

data Arc4 = Arc4 {
      a4i :: !Word8
    , a4j :: !Word8
    , a4s :: !(UArray Word8 Word8)
    }

instance Show Arc4 where
    showsPrec _ a r = "i = " ++ show (a4i a) ++ ", j = " ++ show (a4j a)
                      ++ ", s = " ++ (show $ elems $ a4s a) ++ r
a4virgin :: Arc4
a4virgin = Arc4 0 0 $ listArray (0, 0xff) [0..0xff]

a4new :: String -> Arc4
a4new key = doit a4virgin (map (fromIntegral . ord) key) (0::Int) []
    where
      doit _ [] _ _                = error "a4new: empty key"
      doit a4 _ 0x100 _            = a4 { a4i = 0, a4j = 0 }
      doit a4 k n []               = doit a4 k n k
      doit (Arc4 i j' s) k n (c:t) = doit a4 k (n+1) t
          where j  = j' + c + s!i
                a4 = Arc4 (i+1) j $ s // [(i, s!j), (j, s!i)]

a4byte                 :: Arc4 -> (Word8, Arc4)
a4byte (Arc4 i' j' s') = r `seq` (r, Arc4 i j s)
    where i = i' + 1
          j = j' + s'!i
          s = s' // [(i, s'!j), (j, s'!i)]
          r = s!(s!i + s!j)

data Arc4IO = Arc4IO (IORef (Word8, Word8)) (IOUArray Word8 Word8)

a42io :: Arc4 -> IO Arc4IO
a42io (Arc4 i j s') = do
  ij <- newIORef (i, j)
  s <- thaw s'
  return $ Arc4IO ij s

io2a4 :: Arc4IO -> IO Arc4
io2a4 (Arc4IO ij s') = do
  (i, j) <- readIORef ij
  s <- freeze s'
  return $ Arc4 i j s

unsafeio2a4 :: Arc4IO -> IO Arc4
unsafeio2a4 (Arc4IO ij s') = do
  (i, j) <- readIORef ij
  s <- unsafeFreeze s'
  return $ Arc4 i j s

a4byteIO :: Arc4IO -> IO Word8
a4byteIO (Arc4IO ij s) = do
  (i', j') <- readIORef ij
  let i = i' + 1
  j <- liftM (j' +) $ readArray s i
  writeIORef ij (i, j)
  sj <- readArray s i
  si <- readArray s j
  writeArray s i si
  writeArray s j sj
  b <- readArray s $ si + sj
  return b

a4chunkIO :: Arc4IO -> Int -> IO S.ByteString
a4chunkIO a4 n = S.create n (fill 0)
    where
      fill i _ | i >= n = return ()
      fill i p          = do
        a4byteIO a4 >>= pokeElemOff p i
        fill (i+1) p
        
a4chunk :: Arc4 -> Int -> (S.ByteString, Arc4)
a4chunk a4' n = unsafePerformIO $ do
                 a <- a42io a4'
                 c <- a4chunkIO a n
                 a4 <- unsafeio2a4 a
                 return (c, a4)
                 
a4stringN :: String -> Int -> L.ByteString
a4stringN seed n1 = generate (a4new seed) n1
    where
      generate _ n | n <= 0 = L.Empty
      generate a4' n         = L.chunk c $ generate a4 (n - i)
          where
            i = min L.defaultChunkSize n
            (c, a4) = a4chunk a4' i

type A4Random = MVar Arc4IO

a4RandomNew :: String -> IO A4Random
a4RandomNew seed | null seed = liftM show getClockTime >>= a4RandomNew
a4RandomNew seed             = a42io (a4new seed) >>= newMVar

a4RandomBits :: (MonadIO m, Bits r) => A4Random -> m r
a4RandomBits m = liftIO $ withMVar m (result (bitSize zero) zero)
    where
      zero = 0       -- Here to infer the type a so we can use bitSize
      result k n _ | k <= 0 = return n
      result k n a          =
          do b <- a4byteIO a
             result (k - 8) (shiftL n 8 .|. fromIntegral b) a

a4RandomBool :: (MonadIO m) => A4Random -> Float -> m Bool
a4RandomBool _ prob | prob <= 0.0 = return False
a4RandomBool _ prob | prob >= 1.0 = return True
a4RandomBool m prob               = do
  w <- a4RandomBits m
  return $ (fromIntegral (w :: Word32))
             < (fromIntegral (maxBound :: Word32)) * prob

a4RandomChunkN :: (MonadIO m) => A4Random -> Int -> m S.ByteString
a4RandomChunkN m n = liftIO $ withMVar m (flip a4chunkIO n)

a4RandomStringN :: (MonadIO m) => A4Random -> Int -> m L.ByteString
a4RandomStringN m n =
    liftIO $ do seed <- a4RandomChunkN m 32
                return $ a4stringN (fmap (chr . fromIntegral) $ S.unpack seed) n

{-

a4stringNslow :: String -> Int -> L.ByteString
a4stringNslow seed n1 = L.unfoldr step (a4new seed, n1)
    where
      assoc ((a, b), c) = (a, (b, c))
      step (a, n) | n > 0 = Just $ assoc $ (a4byte a, n - 1)
      step _              = Nothing

test :: (String -> Int -> L.ByteString) -> FilePath -> String -> IO ()
test fn path seed = do
  h <- openBinaryFile path WriteMode
  L.hPut h $ fn seed 10000000
  hClose h

main :: IO ()
main = test a4stringN "crap" "a"

-}
