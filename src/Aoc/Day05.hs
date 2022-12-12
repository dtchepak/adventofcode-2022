{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day05 where

import Data.Char (isAlphaNum)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Control.Applicative (liftA3)
import Control.Monad
import Control.Monad.State.Strict
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

readInput :: IO T.Text
readInput = T.readFile "data/day05.txt"

data Stack a = Stack [a] deriving (Show, Eq)
type Stacks = Map Int (Stack Char)

moveGroup :: Int -> Int -> Int -> State Stacks ()
moveGroup n from to = modify $ \m ->
  let Stack source = fromMaybe (error $ "No stack " ++ show from ) $ Map.lookup from m
      Stack target = fromMaybe (error $ "No stack " ++ show to) $ Map.lookup to m
      (crates, source') = (take n source, Stack (drop n source))
      target' = Stack (crates ++ target)
  in Map.insert from source' . Map.insert to target' $ m

moveOne ::  Int -> Int -> Stacks -> Stacks
moveOne from to =
  execState (moveGroup 1 from to)

move :: Int -> Int -> Int -> State Stacks ()
move n from to =
  replicateM_ n . modify $ moveOne from to

data Move = Move { count:: Int, fromCrate :: Int, toCrate::Int} deriving (Show, Eq)

runMoves :: [Move] -> Stacks -> Stacks
runMoves = execState . traverse_ (\(Move c from to) -> move c from to)

runMoves2 :: [Move] -> Stacks -> Stacks
runMoves2 = execState . traverse_ (\(Move c from to) -> moveGroup c from to)

tops :: Stacks -> [Char]
tops = map (\(_, Stack xs) -> head xs) . Map.toAscList 

answers :: IO ()
answers = do
  input <- readInput
  let initial = unsafeParseStacks input
  let moves = parseMoves input
  putStrLn "Part 1:"
  print $ tops . flip runMoves initial <$> moves
  putStrLn "Part 2:"
  print $ tops . flip runMoves2 initial <$> moves

type Parser = Parsec Void T.Text

-- Example input: "move 1 from 2 to 1"
parseMove :: Parser Move
parseMove = liftA3 Move (string "move " *> L.decimal) (string " from " *> L.decimal) (string " to " *> L.decimal)

parseMoves :: T.Text -> Either String [Move]
parseMoves =
  first show . traverse (parse parseMove "") . filter ("move" `T.isPrefixOf`) . T.lines

{-| Partial function to parse stacks from the input format.

Initial stacks from data/day05.txt:
```
    [V] [G]             [H]        
[Z] [H] [Z]         [T] [S]        
[P] [D] [F]         [B] [V] [Q]    
[B] [M] [V] [N]     [F] [D] [N]    
[Q] [Q] [D] [F]     [Z] [Z] [P] [M]
[M] [Z] [R] [D] [Q] [V] [T] [F] [R]
[D] [L] [H] [G] [F] [Q] [M] [G] [W]
[N] [C] [Q] [H] [N] [D] [Q] [M] [B]
 1   2   3   4   5   6   7   8   9 
 ```

Result:
```
1: [Z, P, B, Q, M, D, N]
2: [V, H, D, M, Q, Z, L, C]
...
9: [M, R, W, B]
```
```
-}
unsafeParseStacks :: T.Text -> Stacks
unsafeParseStacks =
  let stackLines =
        filter (not . T.null)
        . map (T.filter isAlphaNum)
        . T.transpose
        . takeWhile (not . T.null)
        . T.lines
      readStack :: T.Text -> Int
      readStack line = read . pure . T.last $ line
      lineToMapEntry line = (readStack line, Stack . T.unpack . T.init $ line)
  in Map.fromList . fmap lineToMapEntry . stackLines

