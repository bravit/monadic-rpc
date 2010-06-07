{-# LANGUAGE TemplateHaskell #-}
module RFDefs where

import Data.List
import Data.Time
import Control.Monad

import DDefs
import ServerUtils
import DeclsGenerator
import System.IO

$genServerDecls

number :: Integer -> IO ()
number a = appendFile "numbers.txt" (show a ++ "\n")

totalSum :: IO Integer
totalSum = do
    nn <- readFile "numbers.txt"
    let res = sum $ map (\s -> (read s :: Integer)) (lines nn)
    return res
