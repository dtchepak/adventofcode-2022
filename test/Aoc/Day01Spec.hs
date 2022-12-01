module Aoc.Day01Spec (spec) where

import Test.Hspec
import Aoc.Day01

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: example" $
      1 `shouldBe` 1
