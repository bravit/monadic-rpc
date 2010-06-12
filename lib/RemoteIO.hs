{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
module RemoteIO where

import DDefs
import Network
import System.IO
import System.IO.Error
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State


import Data.Serialize hiding (get,put)
import Data.Serialize.Put
import Data.Serialize.Get
import qualified Data.ByteString as BS

runRemote :: RemoteState st => PeerAddr -> RemoteStIO st a -> IO (Either String a)
runRemote peer computation = do
    cfg  <- remoteConnectTo peer
    res <- runRemoteCfg cfg computation
    remoteClose cfg
    return res

runRemoteCfg :: RemoteState st => RemoteConfig -> RemoteStIO st a -> IO (Either String a)
runRemoteCfg cfg computation = 
    runErrorT $ 
        runReaderT (evalStateT (runRem computation) initState) cfg

runRemoteCfg_ cfg computation = runRemoteCfg cfg computation >> return ()


--send :: (Serialize a) => a -> RemoteIO ()
send msg = do
    cfg <- ask
    liftIO $ do 
                BS.hPut (handle cfg) (buildMsgEnv $ encode msg)
                hFlush (handle cfg)
    where
        buildMsgEnv payload = 
            runPut $ do 
                putWord64be size
                putByteString payload
            where size = (fromIntegral $ BS.length payload) :: Word64

receive :: (Serialize a) => RemoteStIO st a
receive = do
    cfg <- ask
    res <-liftIO $ try $ recvMsgEnv (handle cfg)
    case res of 
        Left ioe -> remoteError $ "Protocol error: "++ show ioe
        Right res -> either (remoteError . const "Decoding error (stage 1)") return (decode res)
    where 
        recvMsgEnv h = do
            sz_msg <- BS.hGet h DDefs.msgSizeField
            either fail (BS.hGet h . fromIntegral) (runGet getWord64be sz_msg)

rIsEOF :: RemoteStIO st Bool
rIsEOF = do
    cfg <- ask
    liftIO $ hIsEOF (handle cfg)

--remoteError :: String -> RemoteIO st a
remoteError err_msg = do
    rp <- remotePeerInfo
    throwError $ err_msg ++ rp

remotePeerInfo :: RemoteStIO st String
remotePeerInfo = do
    cfg <- ask
    let peer = remotePeer cfg
    return $ " [" ++ hostname peer ++ ":" ++ show (port peer) ++ "]"

remoteConnectTo :: PeerAddr -> IO RemoteConfig
remoteConnectTo peer = do
    conn <- connectTo (hostname peer) (PortNumber $ port peer)
    return (RemoteConfig (PeerAddr "" 0) peer conn)

remoteClose cfg = hClose (handle cfg)


