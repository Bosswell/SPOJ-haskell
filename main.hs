module Main where

import Data.List
import Control.Monad

isCollinear :: [Int] -> Bool
isCollinear xs
    | length xs /= 6 = False
    | isCollinearX xs = True
    | isCollinearY xs = True
    | hasAtLeastTwoIdenticalPoints (chunksOf 2 xs) = True
    | isCrossCollinear xs = True
    | otherwise = False

isCrossCollinear :: [Int] -> Bool
isCrossCollinear xs
    | xs !! 0 == xs !! 2 = False
    | xs !! 0 == xs !! 4 = False
    | fromIntegral (xs !! 1 - xs !! 3) / fromIntegral (xs !! 0 - xs !! 2) == fromIntegral (xs !! 1 - xs !! 5) / fromIntegral (xs !! 0 - xs !! 4) = True
    | otherwise = False

isCollinearX :: [Int] -> Bool
isCollinearX xs = (xs !! 0 == xs !! 2) && (xs !! 0 == xs !! 4)

isCollinearY :: [Int] -> Bool
isCollinearY xs = (xs !! 1 == xs !! 3) && (xs !! 1 == xs !! 5)

hasAtLeastTwoIdenticalPoints :: [[Int]] -> Bool
hasAtLeastTwoIdenticalPoints xs
    | length xs /= 3 = False
    | xs !! 0 == xs !! 1 = True
    | xs !! 0 == xs !! 2 = True
    | otherwise = False


normalizeInput :: [String] -> [[Int]]
normalizeInput = map (castStringToInt . words)

castStringToInt :: [String] -> [Int]
castStringToInt = map read

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = (take n xs):(chunksOf n (drop n xs))

normalizeOutput :: Bool -> String
normalizeOutput a
    | a = "TAK"
    | otherwise = "NIE"

readLines :: IO [String]
readLines = do
    line <- getLine
    let count :: Int
        count = read line
    lines <- replicateM (count) $ do
        line <- getLine
        return line
    return lines

isCollinearOutput :: [Int] -> String
isCollinearOutput = normalizeOutput . isCollinear

main :: IO ()
main =
  do
    lines <- readLines
    putStrLn (intercalate "\n" (map isCollinearOutput (normalizeInput lines)))

