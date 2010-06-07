module Main where

import ServerUtils
import RFDefs
import Control.Concurrent
import Control.Monad

serve1 = serve 1501 registeredFunctions
serve2 = serve 1502 registeredFunctions
main = forM_ [1501, 1502] (\ p -> forkIO $ serve p registeredFunctions) 
    
