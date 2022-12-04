module Aoc.Day02 (
  Shape,
  Result,
  unsafeGameScore,
  result,
  sumOfScores,
  answers
) where

readInput :: IO String
readInput = readFile "data/day02.txt"

data Shape = Rock | Paper | Scissors deriving (Show, Eq)

data Result = Win | Draw | Loss deriving (Show, Eq)

unsafeParseShape :: Char -> Shape
unsafeParseShape c = case c of
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors
  _   -> error ("Unknown shape: " ++ [c])

unsafeGameScore :: String -> Int
unsafeGameScore (x : ' ' : y : []) = gameScore (unsafeParseShape x) (unsafeParseShape y)
unsafeGameScore s = error ("Unknown game format: " ++ s)

resultScore :: Result -> Int
resultScore Win = 6
resultScore Draw = 3
resultScore Loss = 0

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

-- | Result for player of second shape
result :: Shape -> Shape -> Result
result Rock Rock = Draw
result Rock Paper = Win
result Rock Scissors = Loss

result Paper Rock = Loss
result Paper Paper = Draw
result Paper Scissors = Win

result Scissors Rock = Win
result Scissors Paper = Loss
result Scissors Scissors = Draw

-- | Score for player of second shape
gameScore :: Shape -> Shape -> Int
gameScore theirs ours = shapeScore ours + resultScore (result theirs ours)

sumOfScores :: String -> Int
sumOfScores = sum . fmap unsafeGameScore . lines

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1"
  print (sumOfScores input)
