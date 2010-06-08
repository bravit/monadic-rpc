module Main where

import ServerUtils
import DeclsGenerator
import RFDefs

main :: IO ()
main = serve 1500 registeredFunctions
