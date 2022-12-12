{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day07Spec (spec) where

import Aoc.Day07
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: example 1" $
      1 === 1
