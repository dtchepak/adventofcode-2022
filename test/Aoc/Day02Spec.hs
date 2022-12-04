module Aoc.Day02Spec (spec) where

import Test.Hspec
import Aoc.Day02

spec :: Spec
spec = do
  describe "example data" $ do
    let exampleData = "A Y\nB X\nC Z"
    it "part 1: example A Y" $
      unsafeGameScore "A Y" `shouldBe` 8
    it "part 1: example B X" $
      unsafeGameScore "B X" `shouldBe` 1
    it "part 1: example C Z" $
      unsafeGameScore "C Z" `shouldBe` 6
    it "part 1: example input" $
      sumOfScores exampleData `shouldBe` 15

    it "part 2: example A Y" $
      unsafeGameScore2 "A Y" `shouldBe` 4
    it "part 2: example B X" $
      unsafeGameScore2 "B X" `shouldBe` 1
    it "part 2: example C Z" $
      unsafeGameScore2 "C Z" `shouldBe` 7
    it "part 2: example input" $
      sumOfScores2 exampleData `shouldBe` 12
