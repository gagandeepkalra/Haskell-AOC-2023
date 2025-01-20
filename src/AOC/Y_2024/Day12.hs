{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module AOC.Y_2024.Day12 (solve) where

-- https://adventofcode.com/2024/day/12

import Control.Monad (mfilter)
import Data.Array.Unboxed (IArray, UArray, (!), (!?), array, indices)
import Data.Bifunctor (Bifunctor(first, second))
import Data.Foldable (Foldable(foldl', foldMap'))
import Data.List (unfoldr)
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Data.Set as Set (Set, member, empty, insert, union, notMember, null, size)
import Text.Parsec (newline, sepEndBy, many, parse, letter)

type Grid a = UArray Point a
type Point  = (Int, Int)

mkGrid :: IArray UArray e => [[e]] -> Grid e
mkGrid xs = array ((0, 0), (rows - 1, cols - 1))
    [((i, j), c)
    | (i, r) <- [0 ..] `zip` xs
    , (j, c) <- [0 ..] `zip` r
    ]
  where
    rows = length xs
    cols = length $ head xs

parseInput :: String -> Grid Char
parseInput s = case parse grid "" s of
    Left err -> error (show err)
    Right g -> g
  where
    grid = mkGrid <$> many letter `sepEndBy` newline

deltas :: [Point -> Point]
deltas = [north, south, east, west]

north, south, east, west :: Point -> Point
north = first (subtract 1)
south = first (+ 1)
east  = second (+ 1)
west  = second (subtract 1)

northEast, northWest, southEast, southWest :: Point -> Point
northEast = north . east
northWest = north . west
southEast = south . east
southWest = south . west

type Visited = Set Point
type Component = Set Point

connectedComponents :: Grid Char -> [Component]
connectedComponents grid = filter (not . Set.null) $ unfoldr (uncurry f) (Set.empty, indices grid)
  where
    f :: Visited -> [Point] -> Maybe (Component, (Visited, [Point]))
    f _ [] = Nothing
    f visited (p : ps)
      | p `member` visited = Just (empty, (visited, ps))
      | otherwise =
        let component = dfs empty p
        in Just (component, (visited `union` component, ps))

    dfs :: Visited -> Point -> Visited
    dfs visited p
      | p `member` visited = visited
      | otherwise = foldl' dfs (insert p visited) $ neighbours p

    neighbours p = mapMaybe ((\p' -> p' <$ mfilter (grid ! p ==) (grid !? p')) . ($ p)) deltas

part1 :: Grid Char -> Int
part1 = getSum . foldMap (\c -> Sum $ size c * perimeter c) . connectedComponents
  where 
    perimeter :: Component -> Int
    perimeter component = getSum . foldMap (Sum . openEdges component) $ component

    openEdges :: Component -> Point -> Int
    openEdges component p = length . filter id . fmap (outside . ($ p)) $ deltas
      where outside = flip notMember component

part2 :: Grid Char -> Int
part2 = getSum . foldMap' (\c -> Sum $ size c * sides c) . connectedComponents
  where
    sides :: Component -> Int
    sides component = getSum . foldMap (Sum . corners component) $ component

    corners :: Component -> Point -> Int
    corners component p = length . filter id $
      [ ne && n == e || not ne && n && e -- or when the 2 holes touch diagonally
      , nw && n == w || not nw && n && w
      , se && s == e || not se && s && e
      , sw && s == w || not sw && s && w   
      ]
      where
        [n, s, e, w]     = outside . ($ p) <$> [north, south, east, west]
        [ne, nw, se, sw] = outside . ($ p) <$> [northEast, northWest, southEast, southWest]
        outside = flip notMember component

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 12 ---"
    print $ part1 parsed
    print $ part2 parsed
  where parsed = parseInput input