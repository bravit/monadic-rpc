{-# LANGUAGE TemplateHaskell #-}
module RFunctions where
import DeclsGenerator
import ClientUtils

$genClientDecls

instance RemoteState Integer where
    initState = 0

number :: Integer -> RemoteIO ()

totalSum :: RemoteIO Integer
