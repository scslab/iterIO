
module Main where

import System.Environment
import System.IO

import Data.IterIO
import Data.IterIO.Zlib

usage :: IO ()
usage = do
  prog <- getProgName
  hPutStrLn stderr $ "usage: " ++ prog ++ " [-d]"

main :: IO ()
main = do
  av <- getArgs
  case av of
    -- The decision to put inumG[un]zip the the left or right of |$ is
    -- arbitrary, so we do one of each.
    []     -> enumHandle stdin |$ inumGzip ..| handleI stdout
    ["-d"] -> enumHandle stdin |.. inumGunzip |$ handleI stdout
    _      -> usage

-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
