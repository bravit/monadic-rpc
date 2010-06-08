module ServerUtils where

import Network
import System.IO
import Control.Concurrent
import Data.Serialize
import qualified Data.ByteString as BS

import RemoteIO
import DDefs

type FuncRegistry st = [([Char], Parameters -> RemoteStIO st Result)]

data RemoteFunction st a b = Function (a -> b) | Action (a -> RemoteStIO st b)

run_serialized :: (Serialize a, Serialize b) => RemoteFunction st a b -> Parameters -> RemoteStIO st Result
run_serialized (Function func) params 
    = either 
        (remoteError . ("Decoding error (stage 2): "++)) 
        (return . encode . func) 
        (decode params)
run_serialized (Action action) params
    = either 
        (remoteError . ("Decoding error (stage 2): "++)) 
        (\ pars -> do {res <- action pars; return (encode res)}) 
        (decode params)

serve port funcs = withSocketsDo $
    do
       sock <- listenOn (PortNumber port)
       procRequests sock
    where
        procRequests mastersock = 
              do (connhdl, clientHost, clientPort) <- accept mastersock
                 putStrLn $ "New connection from " ++ (clientInfo clientHost clientPort)
                 forkIO $ do 
                    runRemote 
                        (RemoteConfig 
                            (PeerAddr "localhost" port) 
                            (PeerAddr clientHost clientPort) 
                            connhdl)
                        serveClient
                    return ()
                 procRequests mastersock

        serveClient = do
            (ctx, params) <- receive
            res <- callFunction ctx params
            send res
            more serveClient

        callFunction ctx params = do
             case lookup (oper ctx) funcs of
                Just f -> do 
                        result <- f params
                        return (RespCtx True "", result)
                Nothing -> do
                        return (RespCtx False 
                                    (oper ctx ++ ": unsupported operation"), BS.empty)    
        clientInfo clientHost clientPort = clientHost ++ ":" ++ show clientPort
