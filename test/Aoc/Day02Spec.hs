module Aoc.Day02Spec (spec) where

import Aoc.Day02
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype ArbShape = ArbShape Shape deriving (Show, Eq)

instance Arbitrary ArbShape where
  arbitrary = ArbShape <$> elements (enumFrom (toEnum 0))

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

    prop "equal shapes should be a draw" $
      \(ArbShape shape) -> result shape shape `shouldBe` Draw

    prop "a defeats b -> b defeated by a" $
      \(ArbShape this) (ArbShape other) -> case result other this of
        Draw -> label "draw" $ this === other
        Win ->  label "win"  $ this === defeats other .&&. other === defeatedBy this
        Loss -> label "loss" $ this === defeatedBy other .&&. other === defeats this
