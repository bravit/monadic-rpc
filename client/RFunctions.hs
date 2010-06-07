{-# LANGUAGE TemplateHaskell #-}
module RFunctions where
import DeclsGenerator
import ClientUtils

data PairII = PairII Integer Integer
    deriving Show

$genClientDecls

fun :: PairII -> Distributed PairII

sum' :: (Integer, Integer) -> Distributed Integer

double, negate' :: Integer -> Distributed Integer

lengths :: [Integer]->Distributed (Int, Int)

strlen :: String -> Distributed Int

odd' :: Integer -> Distributed Bool

unsupported :: Integer -> Distributed Integer

time :: Distributed String

number :: Distributed Integer

sum'' :: Integer -> Integer -> Distributed Integer

max3 :: Integer -> Integer -> Integer -> Distributed Integer
