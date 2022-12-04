module Aoc.Day02 (
  Shape,
  Result(..),
  defeats,
  defeatedBy,
  unsafeGameScore,
  result,
  sumOfScores,
  unsafeGameScore2,
  sumOfScores2,
  answers
) where

readInput :: IO String
readInput = readFile "data/day02.txt"

data Shape = Rock | Paper | Scissors deriving (Show, Eq, Enum)

data Result = Win | Draw | Loss deriving (Show, Eq, Enum)

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

defeats :: Shape -> Shape
defeats Rock = Paper
defeats Paper = Scissors
defeats Scissors = Rock

defeatedBy :: Shape -> Shape
defeatedBy Rock = Scissors
defeatedBy Paper = Rock
defeatedBy Scissors = Paper

-- | Result for player of second shape
result :: Shape -> Shape -> Result
result theirs ours
  | theirs == ours = Draw
  | ours == defeats theirs = Win
  | otherwise = Loss

-- | Score for player of second shape
gameScore :: Shape -> Shape -> Int
gameScore theirs ours = shapeScore ours + resultScore (result theirs ours)

sumOfScores :: String -> Int
sumOfScores = sum . fmap unsafeGameScore . lines

-- | Given an opponent's shape and a required result, return the shape we need to play
-- to achieve that result.
shapeForResult :: Shape -> Result -> Shape
shapeForResult theirs Draw = theirs
shapeForResult theirs Win = defeats theirs
shapeForResult theirs Loss = defeatedBy theirs

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
