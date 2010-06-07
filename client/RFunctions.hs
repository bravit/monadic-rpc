{-# LANGUAGE TemplateHaskell #-}
module RFunctions where

import DeclsGenerator
import ClientUtils

data PairII = PairII Integer Integer
    deriving Show

$genClientDecls

fun :: PairII -> RemoteIO PairII

sum' :: (Integer, Integer) -> RemoteIO Integer

double, negate' :: Integer -> RemoteIO Integer

lengths :: [Integer]->RemoteIO (Int, Int)

strlen :: String -> RemoteIO Int

odd' :: Integer -> RemoteIO Bool

unsupported :: Integer -> RemoteIO Integer

time :: RemoteIO String

number :: RemoteIO Integer

sum'' :: Integer -> Integer -> RemoteIO Integer

max3 :: Integer -> Integer -> Integer -> RemoteIO Integer
