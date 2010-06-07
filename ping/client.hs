import ClientUtils
import RFunctions

import System.CPUTime
import System.IO
import Control.Monad.Trans


makePing = do
    t1 <- liftIO getCPUTime
    res <- ping
    t2 <- liftIO getCPUTime
    let t = t2-t1
    return (res, t)

main = runDistributed 
        (ServerAddr "localhost" 1500) 
        makePing >>= putStrLn.show
