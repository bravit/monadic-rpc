{-# LANGUAGE TemplateHaskell #-}
module RFunctions where
import DeclsGenerator
import ClientUtils

$genClientDecls

ping :: RemoteIO String
