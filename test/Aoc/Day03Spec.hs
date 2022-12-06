{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day03Spec (spec) where

import Aoc.Day03
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Maybe
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "example data" $ do
    let exampleData = [
            "vJrwpWtwJgWrhcsFMMfFFhFp",
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
            "PmmdzqPrVvPwwTWBwg",
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
            "ttgJtRGJQctTZtZT",
            "CrZsJsPPZsGzwwsLwLmpwMDw"
          ]
    it "part 1: find common items" $
      mapMaybe (uncurry inBoth . compartments) exampleData === "pLPvts"
    it "part 1: compartment priorities" $
      mapMaybe compartmentPriority exampleData === [16, 38, 42, 22,  20, 19]
    it "part 1: example" $
      part1 (T.unlines exampleData) === 157
    it "part 2: example" $
      part2 (T.unlines exampleData) === 70