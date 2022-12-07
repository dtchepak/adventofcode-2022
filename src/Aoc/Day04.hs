module Aoc.Day04 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

readInput :: IO T.Text
readInput = T.readFile "data/day04.txt"

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1:"
  putStrLn "Part 2:"
