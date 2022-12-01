module Aoc.Day02 (
  answers
) where

readInput :: IO String
readInput = readFile "data/day02.txt"

answers :: IO ()
answers = do
  input <- readInput
  putStrLn input
