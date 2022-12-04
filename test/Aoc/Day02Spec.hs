module Aoc.Day02Spec (spec) where

import Test.Hspec
import Aoc.Day02


spec :: Spec
spec = do
  describe "example data" $ do
    let example = "A Y\nB X\nC Z"
    it "part 1: example A Y" $
      unsafeGameScore "A Y" `shouldBe` 8
    it "part 1: example B X" $
      unsafeGameScore "B X" `shouldBe` 1
    it "part 1: example C Z" $
      unsafeGameScore "C Z" `shouldBe` 6
    it "part 1: example input" $
      sumOfScores example `shouldBe` 15
