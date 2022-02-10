module Main where

import Data.List
import Data.List.Split
import Control.Monad

castToInt :: [String] -> [Int]
castToInt = map read

normalizeInput :: [String] -> [[Int]]
normalizeInput = map (castToInt . words)

f :: [Int] -> Bool
f xs
    | length xs /= 6 = False
    | collinearX xs = True
    | collinearY xs = True
    | hasAtLeastTwoIdenticalPoints (chunksOf 2 xs) = True
    | isCrossCollinear xs = True
    | otherwise = False


isCrossCollinear :: [Int] -> Bool
isCrossCollinear xs
    | xs !! 0 == xs !! 2 = False
    | xs !! 0 == xs !! 4 = False
    | fromIntegral (xs !! 1 - xs !! 3) / fromIntegral (xs !! 0 - xs !! 2) == fromIntegral (xs !! 1 - xs !! 5) / fromIntegral (xs !! 0 - xs !! 4) = True
    | otherwise = False

collinearX :: [Int] -> Bool
collinearX xs = (xs !! 0 == xs !! 2) && (xs !! 0 == xs !! 4)

collinearY :: [Int] -> Bool
collinearY xs = (xs !! 1 == xs !! 3) && (xs !! 1 == xs !! 5)

hasAtLeastTwoIdenticalPoints :: [[Int]] -> Bool
hasAtLeastTwoIdenticalPoints xs
    | length xs /= 3 = False
    | xs !! 0 == xs !! 1 = True
    | xs !! 0 == xs !! 2 = True
    | otherwise = False

readLines :: IO [String]
readLines = do
    line <- getLine
    let count :: Int
        count = read line
    lines <- replicateM (count) $ do
        line <- getLine
        return line
    return lines

main :: IO ()
main =
  do
--    lines <- readLines
--    print (normalizeInput lines)
    print (f [1, 2, 3, 4, 5, 6])
    print (f [1, 3, 1, 4, 1, -3])
    print (f [1, 2, -3, 4, 3, 9])
    print (f [-2, -1, 3, -1, -4, -1])
    print (f [0, 0, 0, 0, 0, 0])

