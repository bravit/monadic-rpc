import ClientUtils
import RFunctions

evalSmth n = do
    n2 <- if n /= 0 
            then double n 
            else unsupported n
    s <- sum' (n,n2)
    res <- lengths [1..n]
    len <- strlen (show n)
    b <- odd' n
    p <- fun (PairII 42 42)
    return (n, n2, s, res, len, b, p)

main = do
    cfg <- remoteConnectTo $ PeerAddr "localhost" 1500
    runRemote cfg (evalSmth 10) >>= putStrLn.show
    remoteClose cfg
