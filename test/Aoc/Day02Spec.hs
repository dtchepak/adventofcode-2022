module Aoc.Day02Spec (spec) where

import Test.Hspec
import Aoc.Day02

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: example input parsing" $
      1 `shouldBe` 1
