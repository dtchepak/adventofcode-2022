{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day04Spec (spec) where

import Aoc.Day04
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Maybe
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "example data" $ do
    let exampleData = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
    it "part 1: 2-4,6-8 not fully contained" $
      fullyContained "2-4,6-8" === pure 0
    it "part 1: 2-8,3-7 is fully contained" $
      fullyContained "2-8,3-7" === pure 1
    it "part 1: [(2-4,6-8), (2-8,3-7)] has one fully contained" $
      fullyContained "2-4,6-8\n2-8,3-7" === pure 1
    it "part 1: example" $
      fullyContained exampleData === pure 2
    it "part 2: example" $
      anyOverlap exampleData === pure 4