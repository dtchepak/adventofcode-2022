{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day06Spec (spec) where

import Aoc.Day06
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: first move example" $
      1 === 1