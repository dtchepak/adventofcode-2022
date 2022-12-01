module Aoc.Day01 (
  parseInput,
  findMax,
  sumTop3,
  answers
) where

import Data.List (sortOn)
import Data.Ord (Down(..))

type ElfCalories = Int

readInput :: IO [[ElfCalories]]
readInput = parseInput <$> readFile "data/day01.txt"

parseInput :: String -> [[ElfCalories]]
parseInput =
  fmap (fmap read) . splitOn (== "") . lines

findMax :: [[ElfCalories]] -> ElfCalories
findMax = maximum . fmap sum

sumTop3 :: [[ElfCalories]] -> Int
sumTop3 = sum . take 3 . sortOn Down . fmap sum

headOr :: Foldable t => a -> t a -> a
headOr = foldr const

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p =
  let add a lists = (a:headOr [] lists) : drop 1 lists
  in foldr (\x -> if p x then ([]:) else add x) []

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1:"
  print (findMax input)
  putStrLn "Part 2:"
  print (sumTop3 input)
