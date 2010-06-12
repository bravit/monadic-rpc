import ClientUtils
import RFunctions
import Control.Monad

evalSum = do
    number 1
    number 5
    number 1
    totalSum

evalSum2 = do
    n1 <- evalSum
    restart
    n2 <- evalSum
    return (n1, n2)

--main = runRemote (PeerAddr "localhost" 1500) evalSum2 >>= putStrLn.show

main = runRemote (PeerAddr "localhost" 1500) (liftM2 (,) evalSum evalSum) >>= putStrLn.show
