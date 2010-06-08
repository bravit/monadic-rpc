import ClientUtils
import RFunctions


evalSum = do
    number 1
    number 5
    number 10
    totalSum

main = do
    cfg <- remoteConnectTo $ PeerAddr "localhost" 1500
    runRemote cfg (evalSum) >>= putStrLn.show
    remoteClose cfg
