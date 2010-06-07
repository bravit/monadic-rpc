module Main where

import ServerUtils
import DeclsGenerator
import RFDefs

main = serve 1500 registeredFunctions
