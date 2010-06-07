{-# LANGUAGE TemplateHaskell #-}
module RFunctions where
import DeclsGenerator
import ClientUtils

$genClientDecls

number :: Integer -> Distributed ()

totalSum :: Distributed Integer
