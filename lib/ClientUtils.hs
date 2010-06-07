{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
module ClientUtils where

import DDefs
import CommonUtils

import qualified Data.ByteString as BS
import Data.Serialize

import Network
import Data.List
import System.IO
import System.IO.Error
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State

import Data.Typeable

data ServerAddr = ServerAddr {
        hostname :: String,
        port :: PortNumber
    }

data DistributedState = DState {
        connhdl :: Handle
    }

newtype Distributed a = Distributed {
        runDistr :: StateT DistributedState (ReaderT ServerAddr (ErrorT String IO)) a
    } deriving (Monad, MonadIO, MonadReader ServerAddr, 
                MonadError String, MonadState DistributedState)

runDistributed servAddr computation = runErrorT $ flip runReaderT servAddr $
        do 

                conn <- liftIO $ try $ connectTo (hostname servAddr) (PortNumber $ port servAddr)
                h <- case conn of
                        Right h -> return h
                        Left ioe  -> throwError $ "Connection error" ++ serverInfo servAddr

                res <- evalStateT (runDistr computation) DState{connhdl=h}
                liftIO $ hClose h
                return res

callRemote operation params = do
    answer <- do
                h <- Control.Monad.State.get
                res <- liftIO $ try (sendrecv $ connhdl h)
                either (distribError . show) return res
    either 
        (distribError . ("Decoding error (stage 1): "++) . show)
        decodeAnswer2 
        (decode answer)
    where 
      sendrecv :: Handle -> IO BS.ByteString
      sendrecv h = do
            BS.hPut h (buildReqMsg operation params)
            hFlush h
            recvMsgEnv h
      buildReqMsg operation params 
            = buildMsgEnv payload
              where payload = encode (ReqCtx operation, encode params) 
      decodeAnswer2 (ctx, res) 
        | ok ctx = either (distribError . const "Decoding error (stage 2)")  return (decode res)
        | otherwise = distribError (excInfo ctx)
                
distribError :: String -> Distributed a
distribError err_msg = do
    server <- ask
    throwError $ err_msg ++ serverInfo server

serverInfo server = " [" ++ hostname server ++ ":" ++ show (port server) ++ "]"
