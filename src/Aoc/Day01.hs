module Aoc.Day01 (
  part1,
  findMax,
  input,
  parseInput,
  part2,
  sumTop3
) where

import Data.List (groupBy, sortOn)
import Data.Ord (Down(..))

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

sumTop3 :: [[Int]] -> Int
sumTop3 = sum . take 3 . sortOn Down . fmap sum

part2 :: IO Int
part2 = sumTop3 <$> input
