module Aoc.Day04 (
  pairFullyContains,
  fullyContained,
  anyOverlap,
  answers
) where

import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text
type PairAssignment = (Range Integer, Range Integer)

data Range a = Range a a deriving (Show, Eq)

pRange :: Parser (Range Integer)
pRange = Range <$> L.decimal <*> (char '-' *> L.decimal)

pRangePair :: Parser PairAssignment
pRangePair = (,) <$> pRange <*> (char ',' *> pRange)

pPairs :: Parser [PairAssignment]
pPairs = pRangePair `sepBy` newline

readInput :: IO T.Text
readInput = T.readFile "data/day04.txt"

parsePairs :: T.Text -> Either String [PairAssignment]
parsePairs = first show . parse pPairs ""

-- | True if first range fully contains the second
contains :: Ord a => Range a -> Range a -> Bool
contains (Range lower upper) (Range lower' upper') =
  lower <= lower' && upper >= upper'

overlaps :: Ord a => Range a -> Range a -> Bool
overlaps (Range lower upper) (Range lower' upper') =
  not (upper < lower' || lower > upper')


pairFullyContains :: PairAssignment -> Bool
pairFullyContains (a, b) = a `contains` b || b `contains` a

fullyContained :: T.Text -> Either String Int
fullyContained t =
  let pairs = parsePairs t
  in length <$> filter pairFullyContains <$> pairs

anyOverlap :: T.Text -> Either String Int
anyOverlap t =
  let pairs = parsePairs t
  in length <$> filter (uncurry overlaps) <$> pairs

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1:"
  print (fullyContained input)
  putStrLn "Part 2:"
  print (anyOverlap input)
