{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Aoc.Day07Spec (spec) where

import Aoc.Day07
import qualified Data.Text as T
import qualified Data.Map as Map
import Text.RawString.QQ
import Test.Hspec
import Test.QuickCheck

exampleData :: T.Text
exampleData = [r|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|]

spec :: Spec
spec = do
  describe "path" $ do
    it "path in" $
      pathIn (toPath "/a/b") "c" === toPath "/a/b/c"
    it "path up" $
      pathUp (toPath "/a/b") === toPath "/a"
  describe "cd" $ do
    it "cd a" $
      path (cd "a" newBrowse) === toPath "/a"
  describe "session" $ do
    it "sample session" $
      let fs = parseSession [r|$ cd /
$ cd abc
$ ls
1 a
2 b
$ cd def
$ ls
dir ghi
10 c|]
      in fmap (fmap size . list fs) ["/abc", "/abc/def"] === [Just 3, Just 10]
  describe "example data" $ do 
    it "part 1: example 1" $
      let fs = parseSession exampleData
      in fmap (fmap size . list fs) ["/d", "/a/e"] === [Just 24933642, Just 584]
