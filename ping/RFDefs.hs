{-# LANGUAGE TemplateHaskell #-}
module RFDefs where

import Data.List
import Data.Time
import Control.Monad

import DDefs
import ServerUtils
import DeclsGenerator

$genServerDecls

instance RemoteState Integer where
    initState = 0

ping :: String
ping = "OK"

ping2 :: RemoteStIO Integer String
ping2 = return "OK"
