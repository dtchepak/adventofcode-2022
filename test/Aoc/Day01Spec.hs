module Aoc.Day01Spec (spec) where

import Test.Hspec
import Aoc.Day01

part1Example :: String
part1Example = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: example input parsing" $
      parseInput part1Example `shouldBe` [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]
    it "part 1: example maximum" $
       findMax (parseInput part1Example) `shouldBe` 24000
