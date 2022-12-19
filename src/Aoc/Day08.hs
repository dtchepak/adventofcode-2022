{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day08 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

data TreeMap

readInput :: IO T.Text
readInput = T.readFile "data/day08.txt"

parseInput :: T.Text -> TreeMap
parseInput = error "todo"

part1 :: TreeMap -> Int
part1 = error "todo"

answers :: IO ()
answers = do
  input <- readInput
  let trees = parseInput input
  putStrLn "Part 1:"
  putStrLn "Part 2:"
