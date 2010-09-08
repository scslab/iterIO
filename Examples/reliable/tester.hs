{-# LANGUAGE ForeignFunctionInterface #-}

module Main (module Main, module Arc4) where

import Control.Concurrent
import Control.Monad.Reader
-- import Data.Char
-- import Data.List
import Data.Word
import Network.Socket
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Arc4
import NetSim
import TM
import Target
import Data.IterIO

data Test = Test {
      testStream :: [ThreadId] -> Targ -> Targ -> TM Bool
    , testTargA :: TM Targ
    , testTargB :: TM Targ
    , testAtoB :: NetSim ()
    , testBtoA :: NetSim ()
    , testDescrip :: String
    }

tests :: [Test]
tests =
    [ Test twoWay spawnTarget spawnOrConnect inumNop inumNop
               "Bi-directionally transfer data"
    , Test pingPong spawnTarget spawnOrConnect inumNop inumNop
               "Ping-pong short messages back and forth"
    , Test pingPong spawnTarget spawnOrConnect excessive inumNop
               "Ping-pong with test for excessive retransmissions"
    , Test oneWay internalTarget spawnOrConnect inumNop inumNop
               "Receiving data from reference implementation"
    , Test flowControl spawnTarget spawnTarget inumNop inumNop
               "Flow control when application doesn't read output"
    , Test oneWay spawnTarget internalTarget inumNop inumNop
               "Sending data to reference implementation"
    , Test twoWay spawnTarget internalTarget inumNop inumNop
               "Bi-directionally interoperating with reference"
    , Test eofTest spawnTarget spawnOrConnect inumNop inumNop
               "Test for proper end-of-file handling"
    , Test twoWay spawnOrConnect spawnTarget (garbage 0.05) (garbage 0.05)
               "Two-way transfer injecting 5% garbage packets"
    , Test oneWay internalTarget spawnOrConnect (reorderer 0.02) inumNop
               "Receiving from reference with 2% reordering"
    , Test twoWay spawnTarget spawnOrConnect (duplicater 0.05) (duplicater 0.05)
               "Two-way transfer with 5% packet duplication"
    , Test twoWay spawnTarget spawnOrConnect
               (badlength 0.02) (truncater 0.02)
               "Two-way transfer with 2% of packets having bad length"
    , Test oneWay spawnTarget spawnOrConnect (dropper 0.02) (dropper 0.02)
               "One-way transfer with 2% packet loss"
    , Test twoWay spawnTarget spawnOrConnect (corrupter 0.02) (corrupter 0.02)
               "Two-way transfer with 2% packet corruption"
    ]

runTest :: Int -> Test -> TM Bool
runTest n test = do
  liftIO $ putStr $ printf "TEST %2d:  %-60s" n (testDescrip test ++ "...")
  a <- testTargA test
  b <- testTargB test
  threads <- mapM forkTM [
                tUSource a |$ testAtoB test .| tUDrain b
               , tUSource b |$ testBtoA test .| tUDrain a ]
  result <- testStream test threads a b
  liftIO $ putStrLn $ if result then "passed" else "FAILED"
  return result

data Options = Options{ optSeed :: String
                      , optDebug :: Bool
                      , optWin :: Word32
                      , optTimeout :: Int
                      , optQuiet :: Bool
                      , optList :: Bool
                      , optTest :: Maybe Int
                      , optGdb :: Bool
                      , optServer :: Bool
                      }
defaultOptions :: Options 
defaultOptions = Options { optSeed = ""
                         , optDebug = False
                         , optWin = 1
                         , optTimeout = 1000
                         , optQuiet = True
                         , optList = False
                         , optTest = Nothing
                         , optGdb = False
                         , optServer = False
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "" ["seed"]
      (ReqArg (\s o -> o { optSeed = s }) "SEED")
       "set random seed to a specific string"
    , Option "d" ["debug"]
      (NoArg (\o -> o { optDebug = True }))
       "enable debugging support"
    , Option "" ["gdb"]
      (NoArg (\o -> o { optGdb = True }))
       "print PID forked processes so you can attach with gdb"
    , Option "v" ["verbose"]
      (NoArg (\o -> o { optQuiet = False }))
       "show reliable program stderr"
    , Option "L" ["list"]
      (NoArg (\o -> o { optList = True }))
       "list available tests"
    , Option "s" ["server"]
      (NoArg (\o -> o { optServer = True }))
       "test server mode"
    , Option "w" ["window"]
      (ReqArg (\s o -> o { optWin = read s}) "SIZE")
       "specify window size"
    , Option "T" ["test"]
      (ReqArg (\t o -> o { optTest = Just $ read t}) "#")
       "run just one test"
    , Option "t" ["timeout"]
      (ReqArg (\s o -> o { optTimeout = read s}) "msec")
       "retransmission timeout"
    ]

doOpt :: IO (Options, [String])
doOpt = do
  argv <- getArgs
  case getOpt RequireOrder options argv of
    (o,n,[]) -> return $ (foldl (flip ($)) defaultOptions o, n)
    (_,_,errs) -> do
          hPutStrLn stderr $ concat errs
          usage

usage :: IO a
usage = do
  prog <- getProgName
  let header = "usage: " ++ prog ++ " [OPTIONS] reliable [reliable OPTIONS]\n"
  hPutStrLn stderr $ usageInfo header options
  exitFailure

rt :: Int -> TM ()
rt n | n <= 0    = return ()
     | otherwise = do bool <- asks tcRnd >>= flip a4RandomBool 0.1
                      liftIO $ putStrLn $ show bool
                      rt (n-1)

showTests :: IO ()
showTests = do putStrLn "\nAvailable tests:\n"
               st (1::Int) tests
               putStrLn ""
    where
      st _ [] = return ()
      st n (t:ts) = do _ <- hPrintf stdout " %2d. %s\n" n (testDescrip t)
                       st (n+1) ts

runTests :: Int -> [Test] -> TM (Int,Int)
runTests _ [] = return (0, 0)
runTests n (t:ts) = do
  result <- runTest n t
  (passed, completed) <- runTests (n + 1) ts
  return (if result then passed + 1 else passed, completed + 1)

main :: IO ()
main = withSocketsDo $ do
         (o, argv) <- doOpt
         when (optList o) $ showTests >> exitSuccess
         when (null argv) usage
         r <- a4RandomNew $ optSeed o
         let config' = TestConfig { tcTarget = argv
                                 , tcDebug = optDebug o
                                 , tcRnd = r
                                 , tcWin = optWin o
                                 , tcTimeout = optTimeout o
                                 , tcQuiet = optQuiet o
                                 , tcGdb = optGdb o
                                 , tcServer = Nothing
                                 }
         config <- if optServer o
                   then do server <- runReaderT startServer config'
                           return config' { tcServer = Just server }
                   else return config'
         hSetBuffering stdout NoBuffering
         case optTest o of
           _ | optList o -> showTests
           Just n | n <= 0 || n > length tests -> showTests
           Just n -> do
             _ <- runReaderT (runTest n $ tests !! (n - 1)) config
             return ()
           Nothing -> do
             (passed, completed) <- runReaderT (runTests 1 tests) config
             putStrLn $ printf "SUMMARY: passed %d/%d" passed completed
