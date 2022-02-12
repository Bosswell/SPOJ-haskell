-- https://pl.spoj.com/problems/BINOMS/
module Main where

import Data.List
import Control.Monad

makeNumerator :: Int -> Int -> [Int]
makeNumerator a b = [a, a - 1 .. a - b + 1]

makeDenominator :: Int -> [Int]
makeDenominator b = [1..b]

multiplyList :: [Int] -> Int
multiplyList [] = 1
multiplyList (x:xs) = x * multiplyList xs

normalizeInput :: [String] -> [(Int, Int)]
normalizeInput = map (castStringListToIntTuple . words)

castStringListToIntTuple :: [String] -> (Int, Int)
castStringListToIntTuple xs = (read $ xs !! 0, read $ xs !! 1)

readLines :: IO [String]
readLines = do
    line <- getLine
    let count :: Int
        count = read line
    lines <- replicateM (count) $ do
        line <- getLine
        return line
    return lines

getBinomial :: (Int, Int) -> Int
getBinomial (a, b) = multiplyList (makeNumerator a b) `div` multiplyList (makeDenominator b)

main :: IO ()
main =
  do
    lines <- readLines
    mapM_ print $ map getBinomial $ normalizeInput lines



