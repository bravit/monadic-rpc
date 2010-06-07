import ClientUtils
import RFunctions

import System.CPUTime
import System.IO
import Control.Monad.Trans


evalSum = do
    number 1
    number 5
    number 10
    totalSum

main = runDistributed 
        (ServerAddr "localhost" 1500) 
        evalSum >>= putStrLn.show
