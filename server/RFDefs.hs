{-# LANGUAGE TemplateHaskell #-}
module RFDefs where

import Data.List
import Data.Time
import Control.Monad
import Control.Monad.Trans

import DDefs
import ServerUtils
import DeclsGenerator


data PairII = PairII Integer Integer
    deriving Show

$genServerDecls

fun :: PairII -> PairII
fun (PairII a b) = (PairII (a+1) (b+1))

sum' :: (Integer, Integer) -> Integer
sum' = uncurry (+)

double :: Integer -> Integer
double = (2*)

negate' :: Integer -> Integer
negate' = ((-1) *)

lengths :: [Integer]->(Int, Int)
lengths xs = (length p1, length p2)
    where (p1, p2) = partition odd xs

strlen :: String -> Int
strlen = length

odd' :: Integer -> Bool
odd' = odd

time :: RemoteIO String
time = liftM show (liftIO getZonedTime)

number :: Integer
number = 42

sum'' :: Integer -> Integer -> Integer
sum'' = (+)

max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = maximum [a,b,c]
