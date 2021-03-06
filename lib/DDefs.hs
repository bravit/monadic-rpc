{-# LANGUAGE GeneralizedNewtypeDeriving,FlexibleInstances #-} 
module DDefs where
import qualified Data.ByteString as BS
import Data.Serialize
import Control.Monad
import Network
import System.IO
import System.IO.Error
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State hiding (put, get)

type Operation = String
type Parameters = BS.ByteString
type Result = BS.ByteString
type Message = BS.ByteString

msgSizeField :: Int
msgSizeField = 8 -- in bytes

data RequestContext = ReqCtx {
        oper::Operation
}

data ResponseContext = RespCtx {
     ok :: Bool,
     excInfo :: String
} deriving Show

instance Serialize RequestContext where
  put (ReqCtx op) = put op
  get = liftM ReqCtx get    

instance Serialize ResponseContext where
  put (RespCtx ok excInfo) = do
         put ok
         put excInfo
  get = liftM2 RespCtx get get


type RequestMessage = (RequestContext, Parameters)

type ResponseMessage = (ResponseContext, Result)


data PeerAddr = PeerAddr {
        hostname :: String,
        port :: PortNumber
    }

data RemoteConfig = RemoteConfig {
        localPeer :: PeerAddr,
        remotePeer :: PeerAddr,
        handle :: Handle
    }

class RemoteState a where
    initState :: a

instance RemoteState () where
    initState = ()

newtype RemoteStIO st a = RemoteIO {
        runRem :: StateT st (ReaderT RemoteConfig (ErrorT String IO)) a
    } deriving (Monad, MonadIO, MonadReader RemoteConfig, 
                MonadError String, MonadState st)

type RemoteIO = RemoteStIO ()
