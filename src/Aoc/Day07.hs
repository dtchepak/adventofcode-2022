{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day07 where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (fold)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read (readEither)

-- | Size of files in a directory (excluding sub-directories)
type DirectFileSize = Int

data Path = Path [T.Text]
  deriving (Show, Eq, Ord)

toPath :: T.Text -> Path
toPath = Path . reverse . filter (not . T.null) . T.splitOn "/"

pathIn :: Path -> T.Text -> Path
pathIn (Path xs) = Path . (: xs)

pathUp :: Path -> Path
pathUp (Path xs) = Path (drop 1 xs)

pathDepth :: Path -> Int
pathDepth (Path xs) = length xs
 
data DirectoryContents = DirectoryContents { size :: DirectFileSize }
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
addFile path d =
  Map.alter (pure . (d <>) . fold) path

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

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1:"
  putStrLn "Part 2:"
