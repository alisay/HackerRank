{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
-- import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'plusMinus' function below.
--
-- The function accepts INTEGER_ARRAY arr as parameter.
--

plusMinus arr = do
    putStrLn (stringify $ plusMinus' arr)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

plusMinus' xs = [positive, negative, zero] where
    total = fromIntegral( Data.List.length xs)
    positive = (fromIntegral . Data.List.length $ Data.List.filter (>0) xs) / total
    negative = (fromIntegral . Data.List.length $ Data.List.filter (<0) xs) / total
    zero = 1 - positive - negative

stringify [] = []
stringify (x:xs) = show x ++ "\n" ++ stringify xs

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    plusMinus arr
