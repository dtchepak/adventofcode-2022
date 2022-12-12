{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day06 where

import Data.List (find, nub)
import qualified Data.Text as T
import qualified Data.Text.IO as T

type PacketSize = Int

readInput :: IO T.Text
readInput = T.readFile "data/day06.txt"

packets :: PacketSize -> T.Text -> [(Int, T.Text)]
packets n = zip [0..] . map (T.take n) . T.tails

isDistinct :: T.Text -> Bool
isDistinct x = (== T.length x) . length . nub . T.unpack $ x

findDistinct :: PacketSize -> T.Text -> Maybe Int
findDistinct n =
  fmap ((+n) . fst) . find (isDistinct . snd) . packets n

marker :: T.Text -> Maybe Int
marker = findDistinct 4

message :: T.Text -> Maybe Int
message = findDistinct 14


answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1:"
  print $ marker input
  putStrLn "Part 2:"
  print $ message input
