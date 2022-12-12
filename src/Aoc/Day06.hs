{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day06 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

readInput :: IO T.Text
readInput = T.readFile "data/day06.txt"

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1:"
  putStrLn "Part 2:"

type Parser = Parsec Void T.Text