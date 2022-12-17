{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day07 where

import Data.List (foldl', isPrefixOf, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Foldable (fold)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read (readEither)

type FileSize = Int

data Path = Path [T.Text]
  deriving (Show, Eq, Ord)

toPath :: T.Text -> Path
toPath = Path . reverse . filter (not . T.null) . T.splitOn "/"

pathIn :: Path -> T.Text -> Path
pathIn (Path xs) = Path . (: xs)

pathUp :: Path -> Path
pathUp (Path xs) = Path (drop 1 xs)

pathIsRoot :: Path -> Bool
pathIsRoot (Path xs) = null xs

pathDepth :: Path -> Int
pathDepth (Path xs) = length xs

paths :: Path -> [Path]
paths (Path p) = Path <$> scanr (:) [] p

-- | True if first Path is the parent of the second.
-- A path is not considered a parent of itself.
isParentOf :: Path -> Path -> Bool
(Path a) `isParentOf` (Path b) = 
  a /= b && a `isPrefixOf` b
 
data DirectoryContents = DirectoryContents { size :: FileSize }
  deriving (Show, Eq)

instance Semigroup DirectoryContents where
  a <> b = DirectoryContents (size a + size b)

instance Monoid DirectoryContents where
  mempty = DirectoryContents 0

type FileSystem = Map Path DirectoryContents

data FsBrowseState = FsBrowseState { path :: Path, fs :: FileSystem }
  deriving (Show, Eq)

newBrowse :: FsBrowseState
newBrowse = FsBrowseState (Path []) Map.empty

addFile :: Path -> DirectoryContents -> FileSystem -> FileSystem
addFile path d fs =
  let addPath = Map.alter (pure . (d <>) . fold)
  in foldr addPath fs (paths path)

cd :: T.Text -> FsBrowseState -> FsBrowseState
cd dir s =
  if dir == "/" then s { path = Path [] }
  else if dir == ".." then s { path = pathUp (path s) }
  else s { path = pathIn (path s) dir }

lsEntry :: DirectoryContents -> FsBrowseState -> FsBrowseState
lsEntry d state = state { fs = addFile (path state) d (fs state) }

readContents :: T.Text -> DirectoryContents
readContents t = either (\e -> error $ e ++ ' ' : show t) DirectoryContents . readEither . T.unpack $ t

parseLine :: [T.Text] -> FsBrowseState -> FsBrowseState
parseLine ["$", "cd", x] = cd x
parseLine ["$", "ls"] = id
parseLine ["dir", _] = id
parseLine [fileSize, _] = lsEntry . readContents $ fileSize
parseLine x = error $ "unrecognised line: " ++ show x

list :: FileSystem -> T.Text -> Maybe DirectoryContents
list fs t = Map.lookup (toPath t) fs

parseSession :: T.Text -> FileSystem
parseSession =
  fs . foldl' (flip (parseLine . T.words)) newBrowse .  T.lines

readInput :: IO T.Text
readInput = T.readFile "data/day07.txt"

part1 :: FileSystem -> Int
part1 = sum . filter (<= 100000) . map (size . snd) . Map.toList

part2 :: FileSystem -> Int
part2 fs =
  let requiredSpace = 30000000
      totalDisk = 70000000
      totalUsed = size (fromJust (list fs "/"))
      freeDisk = totalDisk - totalUsed
      toDelete = requiredSpace - freeDisk
  in head . sort . filter (>= toDelete) . map (size . snd) . Map.toList $ fs

answers :: IO ()
answers = do
  input <- readInput
  let session = parseSession input
  putStrLn "Part 1:"
  print $ part1 session
  putStrLn "Part 2:"
  print $ part2 session
