{-# LANGUAGE ScopedTypeVariables #-}
module AOC.Y_2024.Day05 (solve) where

-- https://adventofcode.com/2024/day/5

import Data.Bifunctor ( second, bimap )
import Data.Foldable ( foldl' )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set ( Set, insert, intersection, member, singleton, union, empty, fromList )
import Text.Parsec ( parse, char, endBy, newline, sepBy, many1, digit )

type Graph a = Map.Map a (Set a)

mkGraph :: Ord a => [(a, a)] -> Graph a
mkGraph = Map.fromListWith (<>) . fmap (second singleton)

subGraph :: Ord a => Graph a -> Set a -> Graph a
subGraph graph nodes =
    Map.map (intersection nodes)
  . Map.filterWithKey (\k _ -> k `member` nodes)
  $ graph

neighbours :: Ord a => Graph a -> a -> Set a
neighbours graph x = fromMaybe empty $ Map.lookup x graph

-- | Perform a topological sort on a directed acyclic graph (DAG).
topologicalSort :: forall a . Ord a => Graph a -> [a]
topologicalSort graph = fst . foldl' go ([], empty) $ Map.keys graph
  where
    go :: ([a], Set a) -> a -> ([a], Set a)
    go acc@(_, seen) x
      | x `member` seen = acc
      | otherwise = bimap (x :) (insert x) . foldl' go acc $ neighbours graph x

type Inputs  = [[Int]]
type Problem = (Graph Int, Inputs)

parseInputs :: String -> Problem
parseInputs s = case parse input "" s of
    Left err -> error (show err)
    Right g -> g
  where
    input = do
      let int  = read <$> many1 digit
          pair = (,) <$> int <*> (char '|' *> int)
      graph <- fmap mkGraph $ pair `endBy` newline
      _ <- newline
      let row = int `sepBy` char ','
      rows <- row `sepBy` newline
      return (graph, rows)

middle :: [a] -> a
middle ls = ls !! (length ls `div` 2)

isValid :: forall t a . (Foldable t, Ord a) => Graph a -> t a -> Bool
isValid graph = fst . foldr f (True, empty)
  where
    f :: a -> (Bool, Set a) -> (Bool, Set a)
    f x acc@(correct, seen)
      | not correct = acc
      | x `member` seen = (False, seen)
      | otherwise = (True, seen `union` neighbours graph x)

part1 :: Problem -> Int
part1 (graph, rows) = foldl' f 0 rows
  where
    f acc r = if isValid graph r then acc + middle r else acc

part2 :: Problem -> Int
part2 (graph, rows) = foldl' f 0 rows
  where
    f acc r = if isValid graph r then acc else acc + middle (fix r)
    fix = topologicalSort . subGraph graph . fromList

solve :: String -> IO ()
solve input = putStrLn "--- Day 05 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseInputs