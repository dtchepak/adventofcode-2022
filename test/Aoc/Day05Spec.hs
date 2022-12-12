{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day05Spec (spec) where

import Aoc.Day05
import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Map as Map

stacks :: [T.Text] -> Stacks
stacks = Map.fromList . zip [1..] . map (Stack . T.unpack)

exampleInput :: [T.Text]
exampleInput =
  [ "    [D]    "
  , "[N] [C]    "
  , "[Z] [M] [P]"
  , " 1   2   3 "
  , ""
  , "move 1 from 2 to 1"
  , "move 3 from 1 to 3"
  , "move 2 from 2 to 1"
  , "move 1 from 1 to 2"
  ]

spec :: Spec
spec = do
  describe "example data" $ do
    let exampleData = stacks [ "NZ", "DCM", "P" ]
    let moves = [Move 1 2 1, Move 3 1 3, Move 2 2 1, Move 1 1 2 ]
    it "part 1: first move example" $
      runMoves [Move 1 2 1] exampleData === stacks ["DNZ", "CM", "P"]
    it "part 1: all moves example" $
      runMoves moves exampleData === stacks ["C", "M", "ZNDP"]
    it "part 1: parse moves" $
      let input = "move 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
      in parseMoves input === Right moves
    it "part 1: parse example input" $
      parseMoves (T.unlines exampleInput) === Right moves
    it "part 1: tops all moves example" $
      tops (runMoves moves exampleData) === "CMZ"