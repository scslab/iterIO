
import Control.Monad
import Control.Monad.Trans
import System.IO

import Data.IterIO

logger :: (MonadIO m) => Iter String m ()
logger = do
  (Chunk t eof) <- chunkI
  liftIO $ hPutStrLn stderr $ "Got " ++ show (length t) ++ " bytes\n"
  unless eof logger

main :: IO ()
main = enumStdin
       |. inumTee logger
       |$ (stdoutI :: Iter String IO ())
  
