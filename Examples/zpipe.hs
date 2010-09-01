
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
    []     -> enumHandle stdin |.. inumGzip |$ handleI stdout
    ["-d"] -> enumHandle stdin |$ inumGunzip ..| handleI stdout
    _      -> usage


-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
