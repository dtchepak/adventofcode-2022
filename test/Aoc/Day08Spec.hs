{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Aoc.Day08Spec (spec) where

import Aoc.Day08
import qualified Data.Text as T
import Text.RawString.QQ
import Test.Hspec
import Test.QuickCheck

exampleData :: T.Text
exampleData = [r|30373
25512
65332
33549
35390|]

spec :: Spec
spec = do
  describe "example data" $ do 
    let trees = parseInput exampleData
    it "part 1: example 1" $
      part1 trees === 21

