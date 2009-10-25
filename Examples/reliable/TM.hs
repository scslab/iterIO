
module TM where

import Control.Concurrent
import Control.Monad.Reader
import Data.Word
import Network.Socket

import Arc4
import Data.IterIO.Extra

sendStr :: (SendRecvString t) => Socket -> t -> IO Int
sendStr s t = genSendTo s t Nothing


data ServerProc = ServerProc {
      spAddr :: SockAddr
    , spListenSock :: Socket
    , spKill :: IO ()
    }

data TestConfig = TestConfig { tcTarget :: [FilePath]
                             , tcDebug :: Bool
                             , tcRnd :: A4Random
                             , tcWin :: Word32
                             , tcTimeout :: Int
                             , tcQuiet :: Bool
                             , tcGdb :: Bool
                             , tcServer :: Maybe ServerProc
                             }

type TM = ReaderT TestConfig IO

forkTM :: TM a -> TM ThreadId
forkTM m = ReaderT $ \r -> do
             forkIO $ runReaderT m r >> return ()
    
