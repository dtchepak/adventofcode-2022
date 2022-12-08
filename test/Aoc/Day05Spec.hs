{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day05Spec (spec) where

import Aoc.Day05
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Maybe
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "example data" $ do
    let exampleData = ""
    it "part 1: example" $
      1 == 1