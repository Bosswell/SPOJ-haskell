-- https://pl.spoj.com/problems/BINOMS/
module Main where

import Data.List
import Control.Monad

makeNumerator :: Int -> Int -> [Int]
makeNumerator a b
        | a == b = [1]
        | a - b >= 2 * b = [a, a - 1 .. a - b + 1]
        | a - b < b = [b + 1 .. a]

makeDenominator :: Int -> Int -> [Int]
makeDenominator a b
        | a == b = [1]
        | a - b >= b = [2 .. b]
        | a - b < b = [2 .. a - b]

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

makeNumeratorUsingPrimes :: Int -> Int -> [Int]
makeNumeratorUsingPrimes a = toPrimeFactors . makeNumerator a

makeDenominatorUsingPrimes :: Int -> Int -> [Int]
makeDenominatorUsingPrimes a = toPrimeFactors . makeDenominator a

getBinomial :: (Int, Int) -> Int
getBinomial (a, b) = multiplyList $ (makeNumeratorUsingPrimes a b) \\ (makeDenominatorUsingPrimes a b)

primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

toPrimeFactors :: [Int] -> [Int]
toPrimeFactors [] = []
toPrimeFactors (x:xs) = primeFactors x ++ toPrimeFactors xs

main :: IO ()
main =
  do
    lines <- readLines
    mapM_ print $ map getBinomial $ normalizeInput lines