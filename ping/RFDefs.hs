{-# LANGUAGE TemplateHaskell #-}
module RFDefs where

import Data.List
import Data.Time
import Control.Monad

import DDefs
import ServerUtils
import DeclsGenerator

$genServerDecls

ping :: String
ping = "OK"
