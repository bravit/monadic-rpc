{-# LANGUAGE TemplateHaskell #-}
module RFunctions where
import DeclsGenerator
import ClientUtils

$genClientDecls

register :: String -> Distributed ()

post :: String -> Distributed ()

read :: Integer -> Distributed (String, Integer)
