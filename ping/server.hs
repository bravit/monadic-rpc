module Main where

import ServerUtils
import RFDefs
import Network

main = serve 1500 registeredFunctions
    
