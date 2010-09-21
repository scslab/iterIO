
module Data.IterIO.Search (inumStopString
                          , mapI, mapLI
                          ) where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.Search as Search
import qualified Data.ListLike as LL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Data.IterIO.Base
import Data.IterIO.Inum

-- | Feeds input to an Iteratee until some boundary string is found.
inumStopString :: (Monad m) =>
                  S8.ByteString
               -> Inum L8.ByteString L8.ByteString m a
inumStopString spat = mkInumM $ nextChunk L8.empty
    where
      lpat = L8.fromChunks [spat]
      plen = toEnum $ S8.length spat
      search = Search.breakOn spat
      nextChunk old = do
        (Chunk t eof) <- lift chunkI
        case search $ L8.append old t of
          (a, b) | not (L8.null b) -> do lift $ Done () (chunk $ b)
                                         ifeed a
          (a, _) | eof             -> ifeed a
          (a, _)                   -> checkEnd a
      checkEnd t = let tlen = L8.length t
                       hlen = max 0 (tlen - plen - 1)
                       ttail = L8.drop hlen t
                       fpm = firstPossibleMatch 0 ttail
                       rlen = hlen + fpm
                   in if rlen == tlen
                      then ifeed t >> nextChunk L8.empty
                      else case L8.splitAt rlen t of
                             (r, o) -> ifeed r >> nextChunk o
      firstPossibleMatch n t =
          if t `L8.isPrefixOf` lpat
          then n
          else firstPossibleMatch (n + 1) (L8.tail t)

longestCommonPrefix :: (LL.ListLike t e, Eq e) => t -> t -> t
longestCommonPrefix a0 = cmp 0 a0
    where
      cmp n a b | LL.null a || LL.null b = LL.take n a0
      cmp n a b | LL.head a == LL.head b = cmp (n + 1) (LL.tail a) (LL.tail b)
      cmp n _ _                          = LL.take n a0

findLongestPrefix :: (LL.ListLike t e, Ord t, Eq e) =>
                     Map t a -> t -> Maybe (t, a)
findLongestPrefix mp t = maybe ckprefix (\v1 -> Just (t, v1)) ma
    where
      (ltmap, ma, _) = Map.splitLookup t mp
      (k, v) = Map.findMax ltmap
      p = longestCommonPrefix k t
      ckprefix | Map.null mp || LL.null t = Nothing
               | k `LL.isPrefixOf` t      = Just (k, v)
               | otherwise                = findLongestPrefix ltmap p

-- | Reads input until it can uniquely determine the longest key in a
-- 'Map.Map' that is a prefix of the input.  Consumes the input that
-- matches the key, and returns the corresponding value in the
-- 'Map.Map', along with the residual input that follows the key.
mapI :: (ChunkData t, LL.ListLike t e, Ord t, Eq e, Monad m) =>
        Map t a -> Iter t m a
mapI mp | Map.null mp = throwI $ IterMiscParseErr "mapI: null map"
        | otherwise = do
  c@(Chunk t eof) <- chunkI
  if not (eof) && more t
    then iterF (feedI (mapI mp) . mappend c)
    else case findLongestPrefix mp t of
           Nothing -> throwI $ IterExpected (chunkShow t) $
                      map chunkShow $ Map.keys mp
           Just (k, v) -> Done v (Chunk (LL.drop (LL.length k) t) eof)
    where
      gtmap t = snd $ Map.split t mp
      more t | Map.null $ gtmap t = False
             | otherwise = t `LL.isPrefixOf` (fst $ Map.findMin $ gtmap t)

-- | @mapLI@ is a variant of 'mapI' that takes a list of
-- @(key, value)@ pairs instead of a 'Map.Map'.
-- @mapLI = 'mapI' . 'Map.fromList'@.
mapLI :: (ChunkData t, LL.ListLike t e, Ord t, Eq e, Monad m) =>
         [(t, a)] -> Iter t m a
mapLI = mapI . Map.fromList




{-
main :: IO ()
main = enumStdin |$ do
         inumStopString end .| stdoutI
         match end
         liftIO $ putStrLn "\n\n*** We have reached THE END #1 ***\n\n"
         inumStopString end .| stdoutI
         match end
         liftIO $ putStrLn "\n\n*** We have reached THE END #2 ***\n\n"
         stdoutI
    where
      end = L8.pack "TheEnd"
-}
