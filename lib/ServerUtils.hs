module ServerUtils where

import DDefs
import CommonUtils

import Data.Serialize
import qualified Data.ByteString as BS
import Network
import System.IO
import System.IO.Error

import Control.Concurrent


run_serialized :: (Serialize a, Serialize b) => DistributedFunction a b -> Parameters -> IO Result
run_serialized (Function func) params 
    = either 
        (fail . ("Decoding error (stage 2): "++)) 
        (return . encode . func) 
        (decode params)
run_serialized (Action action) params
    = either 
        (fail . ("Decoding error (stage 2): "++)) 
        (\ pars -> do {res <- action pars; return (encode res)}) 
        (decode params)

serve :: PortNumber -> FunctionsRegistry -> IO ()
serve port funcs = withSocketsDo $
    do
       sock <- listenOn (PortNumber port)
       procRequests sock

    where
          procRequests mastersock = 
              do (connhdl, clientHost, clientPort) <- accept mastersock
                 putStrLn $ "New connection from " ++ (clientInfo clientHost clientPort)
                 forkIO $ procMessages connhdl clientHost clientPort
                 procRequests mastersock

          procMessages connhdl clientHost clientPort = do
                     message <- recvMsgEnv connhdl
                     either (fail . ("Decoding error (stage 1):" ++)) callOper (decode message)
                     eof <- hIsEOF connhdl
                     if eof
                        then return ()
                        else procMessages connhdl clientHost clientPort
                 where
                    callOper (ctx, params) = do
                         putStrLn $ "Call " ++ oper ctx
                         case lookup (oper ctx) funcs of
                            Just f -> do 
                                    result <- f params
                                    send_response $ (RespCtx True "", result)
                            Nothing -> send_response (RespCtx False 
                                                (oper ctx ++ ": unsupported operation"), "")
                    send_response resp = do
                            BS.hPut connhdl (buildRespMsg resp)
                            hFlush connhdl
                    buildRespMsg resp = buildMsgEnv (encode resp)

clientInfo clientHost clientPort = show clientHost ++ ":" ++ show clientPort
