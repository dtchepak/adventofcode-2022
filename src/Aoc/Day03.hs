module Aoc.Day03 (
  compartments,
  inBoth,
  compartmentPriority,
  part1,
  part2,
  groupsOf,
  inAll,
  answers
) where

import Data.Char (ord)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

readInput :: IO T.Text
readInput = T.readFile "data/day03.txt"

compartments :: T.Text -> (T.Text, T.Text)
compartments t = T.splitAt (T.length t `div` 2) t

inBoth :: T.Text -> T.Text -> Maybe Char
inBoth t = T.find (`T.elem` t)

priority :: Char -> Int
priority c
  | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
  | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = error $ "unexpected char: " ++ show c

compartmentPriority :: T.Text -> Maybe Int
compartmentPriority = fmap priority . uncurry inBoth . compartments

part1 :: T.Text -> Int
part1 = sum . Maybe.mapMaybe compartmentPriority . T.lines

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf i xs = take i xs : groupsOf i (drop i xs)

inAll :: [T.Text] -> Maybe Char
inAll [] = Nothing
inAll (x:xs) = T.find (\c -> all (c `T.elem`) xs) x

part2 :: T.Text -> Int
part2 = sum . Maybe.mapMaybe (fmap priority . inAll) . groupsOf 3 . T.lines

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1:"
  print (part1 input)
  putStrLn "Part 2:"
  print (part2 input)
