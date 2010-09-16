
module Data.IterIO.Search (inumStopString
                          ) where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.Search as Search

import Data.IterIO

-- | Feeds input to an Iteratee until some boundary string is found.
inumStopString :: (Monad m) =>
                  L8.ByteString
               -> Inum L8.ByteString L8.ByteString m a
inumStopString pat0 = mkInumM $ nextChunk L8.empty
    where
      spat = Search.strictify pat0
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
