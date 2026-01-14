{-# LANGUAGE TupleSections #-}
module AOC.Y_2025.Day08 (solve) where

-- https://adventofcode.com/2025/day/8

import Data.Foldable (foldl')
import Data.List (sortBy, sortOn, tails)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Set as Set (Set, empty, insert, member, size, union)
import Text.Parsec (char, digit, many1, newline, parse, sepEndBy)

-- TODO: UNION FIND ALGORITHM

-- Adjacency list representation of an acyclic graph
type Graph a = Map.Map a [a]

emptyGraph :: Graph a
emptyGraph = Map.empty

neighbours :: Ord a => Graph a -> a -> [a]
neighbours graph x = fromMaybe [] $ Map.lookup x graph

-- return updated graph if the link is valid, otherwise Nothing
-- graph is acyclic, so we can use a simple DFS to check if a path exists between two nodes
link :: Ord a => Graph a -> (a, a) -> Maybe (Graph a)
link graph (t1, t2)
  | reachable graph t1 t2 = Nothing
  | otherwise
    = Just
    . Map.alter (Just . (t2 :) . fromMaybe []) t1
    . Map.alter (Just . (t1 :) . fromMaybe []) t2
    $ graph

-- check if there is a path between two nodes
reachable :: Ord a => Graph a -> a -> a -> Bool
reachable graph frm to = go frm frm
  where
    go p x = x == to || any (go x) (filter (p/=) $ neighbours graph x)

dfs :: Ord a => Graph a -> Set a -> a -> Set a
dfs graph seen x
  | x `member` seen = seen
  | otherwise = foldl (dfs graph) (insert x seen) (neighbours graph x)

connectedComponents :: Ord a => Graph a -> [Set a]
connectedComponents graph = snd . foldl f (empty, []) $ Map.keys graph
  where
    f r@(seen, acc) x
      | x `member` seen = r
      | otherwise = (seen `union` component, component : acc)
        where component = dfs graph empty x

type Triple = (Int, Int, Int)
type Problem = [Triple]

parseInputs :: String -> Problem
parseInputs s = case parse triples "" s of
    Left err -> error (show err)
    Right g -> g
  where
    triples = triple `sepEndBy` newline
    triple = (,,) <$> int <*> (char ',' *> int) <*> (char ',' *> int)
    int  = read <$> many1 digit

-- Euclidean distance
distance :: Triple -> Triple -> Double
distance (x1, y1, z1) (x2, y2, z2) =
  sqrt . fromIntegral $ (x1 - x2)^p + (y1 - y2)^p + (z1 - z2)^p
  where p = 2 :: Int

closest :: [Triple] -> [(Triple, Triple)]
closest xs = sortOn (uncurry distance) $ [ (x,y) | (x:ys) <- tails xs, y <- ys ]

-- make 1000 shortest connections
part1 :: Problem -> Int
part1 = product . take 3 . sortBy (comparing Down) . fmap Set.size
  . connectedComponents
  . foldl link' emptyGraph
  . take 1000
  . closest
  where link' g = fromMaybe g . link g

-- the last link added
part2 :: Problem -> Int
part2 = result . snd
  . foldl' (\acc@(g, _) p -> maybe acc (,p) $ link g p) (emptyGraph, ((0,0,0),(0,0,0)))
  . closest
  where result ((x1,_,_), (x2,_,_)) = x1 * x2

solve :: String -> IO ()
solve input = putStrLn "--- Day 08 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseInputs