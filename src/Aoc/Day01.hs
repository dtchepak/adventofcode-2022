module Aoc.Day01 (
  part1,
  findMax,
  input,
  parseInput
) where

import Data.List (groupBy)

part1 :: IO Int
part1 = findMax <$> input

findMax :: [[Int]] -> Int
findMax = maximum . fmap sum

parseInput :: String -> [[Int]]
parseInput =
  let readSubList = fmap read . filter (not . null)
  in filter (not . null) . fmap readSubList . groupBy (\a b -> a /= "" && b /= "") . lines

input :: IO [[Int]]
input =
  parseInput <$> readFile "data/day01.txt"
