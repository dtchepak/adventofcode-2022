{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day06Spec (spec) where

import Aoc.Day06
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: example 1" $
      marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" === Just 7
    it "part 1: example 2" $
      marker "bvwbjplbgvbhsrlpgdmjqwftvncz" === Just 5
    it "part 1: example 3" $
      marker "nppdvjthqldpwncqszvftbrmjlhg" === Just 6
    it "part 1: example 4" $
      marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" === Just 10
    it "part 1: example 5" $
      marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" === Just 11