{-# LANGUAGE TemplateHaskell,FlexibleInstances #-}
module RFDefs where

import Data.List
import Data.Time
import qualified Control.Monad.State as St

import DDefs
import ServerUtils
import DeclsGenerator
import System.IO

$genServerDecls

instance RemoteState Integer where
    initState = 0

instance RemoteState [Integer] where
    initState = []

number :: Integer -> RemoteStIO [Integer] ()
number a = do
    ns <- St.get
    St.put $ a:ns

totalSum :: RemoteStIO [Integer] Integer
totalSum = do
    ns <- St.get
    St.put []
    return $ sum ns
