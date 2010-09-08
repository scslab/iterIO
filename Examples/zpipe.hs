
module Main where

import System.Environment
import System.IO

import Data.IterIO
import Data.IterIO.Zlib

main :: IO ()
main = do
  av <- getArgs
  hSetBuffering stdout NoBuffering
  case av of
    -- The decision to put inumG[un]zip to the left or to the right
    -- of |$ is arbitrary, so we do one of each.
    []     -> enumHandle stdin |$ inumGzip .| handleI stdout
    ["-d"] -> enumHandle stdin |. inumRepeat inumGunzip |$ handleI stdout
    _      -> do prog <- getProgName
                 hPutStrLn stderr $ "usage: " ++ prog ++ " [-d]"

-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
