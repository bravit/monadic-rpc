{-# LANGUAGE TupleSections #-}

module ServerUtils where

import Network
import System.IO
import Control.Concurrent
import Data.Serialize
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Loops

import RemoteIO
import DDefs

type RemoteAction st a b = a -> RemoteStIO st b

run_serialized :: (Serialize a, Serialize b) => RemoteAction st a b -> Parameters -> RemoteStIO st Result
run_serialized action params
            = either 
                (remoteError . ("Decoding error (stage 2): "++)) 
                (liftM encode . action) 
                (decode params)

serve port funcs = withSocketsDo $
       listenOn (PortNumber port) >>= forever . procRequests
    where
        procRequests mastersock = do 
                (connhdl, clientHost, clientPort) <- accept mastersock
                logConnection clientHost clientPort
                forkIO $ catch (runRemoteCfg_ 
                           (RemoteConfig 
                               (PeerAddr "localhost" port) 
                               (PeerAddr clientHost clientPort) 
                               connhdl)
                           (untilM_ serveClient rIsEOF)) (\ioe -> putStrLn $ show ioe)
            
        serveClient = receive >>= call >>= send

        call (ctx, params) = maybe 
                                (return $ unsupported ctx) 
                                (\f -> liftM (RespCtx True "",) $ f params)
                                (lookup (oper ctx) funcs)

        unsupported ctx = (RespCtx False (oper ctx ++ ": unsupported operation"), BS.empty)

        logConnection clientHost clientPort = 
            putStrLn $ "New connection from " ++ clientHost ++ ":" ++ show clientPort
