module Aoc.Day02 (
  Shape,
  Result,
  unsafeGameScore,
  result,
  sumOfScores,
  unsafeGameScore2,
  sumOfScores2,
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

unsafeParseResult :: Char -> Result
unsafeParseResult c = case c of
  'X' -> Loss
  'Y' -> Draw
  'Z' -> Win
  _   -> error ("Unknown result: " ++ [c])

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

-- | Given an opponent's shape and a required result, return the shape we need to play
-- to achieve that result.
shapeForResult :: Shape -> Result -> Shape
shapeForResult Rock Win = Paper
shapeForResult Rock Draw = Rock
shapeForResult Rock Loss = Scissors

shapeForResult Paper Win = Scissors
shapeForResult Paper Draw = Paper
shapeForResult Paper Loss = Rock

shapeForResult Scissors Win = Rock
shapeForResult Scissors Draw = Scissors
shapeForResult Scissors Loss = Paper

unsafeGameScore2 :: String -> Int
unsafeGameScore2 (x : ' ' : y : []) = 
  let theirShape = unsafeParseShape x
      requiredResult = unsafeParseResult y
      ourShape = shapeForResult theirShape requiredResult
  in gameScore theirShape ourShape
unsafeGameScore2 s = error ("Unknown game format: " ++ s)

sumOfScores2 :: String -> Int
sumOfScores2 = sum . fmap unsafeGameScore2 . lines

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1"
  print (sumOfScores input)
  putStrLn "Part 2"
  print (sumOfScores2 input)
