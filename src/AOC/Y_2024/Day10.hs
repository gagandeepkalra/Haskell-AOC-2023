{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE RankNTypes #-}
module AOC.Y_2024.Day10 (solve) where

-- https://adventofcode.com/2024/day/10

import Control.Monad (mfilter)
import Data.Array.Unboxed (UArray, (!), (!?), array, assocs)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Text.Parsec (digit, newline, sepEndBy, many, parse)

import qualified Data.Set as Set

type Grid a = UArray Point a
type Point  = (Int, Int)

mkGrid :: [[Int]] -> Grid Int
mkGrid xs = array ((0, 0), (rows - 1, cols - 1))
    [((i, j), c)
    | (i, r) <- [0 ..] `zip` xs
    , (j, c) <- [0 ..] `zip` r
    ]
  where
    rows = length xs
    cols = length $ head xs

parseInput :: String -> Grid Int
parseInput s = case parse grid "" s of
    Left err -> error (show err)
    Right g -> g
  where
    grid = mkGrid <$> many element `sepEndBy` newline
    element = digitToInt <$> digit

part1 :: Grid Int -> Int
part1 = solveWith Set.singleton Set.size

part2 :: Grid Int -> Int
part2 = solveWith (const $ Sum 1) getSum

solveWith :: forall a . Monoid a => (Point -> a) -> (a -> Int) -> Grid Int -> Int
solveWith f g grid = getSum . foldMap (Sum . g . eval . fst) . filter ((== 0) . snd) $ assocs grid
  where
    eval p 
      | v == 9 = f p
      | otherwise = foldMap eval neighbours
      where 
        v = grid ! p
        neighbours = mapMaybe ((\p' -> p' <$ mfilter (== v + 1) (grid !? p')) . (p `plus`)) deltas

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (x, y) = bimap (+ x) (+ y)

deltas :: [Point]
deltas = [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= b, a + b /= 0]

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 10 ---"
    print $ part1 parsed
    print $ part2 parsed
  where parsed = parseInput input