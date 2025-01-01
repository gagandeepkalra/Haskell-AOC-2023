module AOC.Y_2024.Day04 (solve) where

-- https://adventofcode.com/2024/day/4

import Text.Parsec ( many, parse, letter, newline, sepEndBy )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Monoid ( Sum(Sum, getSum) )
import Data.Array ( Ix(inRange), (!), array, assocs, bounds, Array )
import Control.Monad (guard)

type Grid = Array (Int, Int) Char
type Directions = [[(Int, Int)]]

toGrid :: [[Char]] -> Grid
toGrid xs = array ((0, 0), (rows - 1, cols - 1)) [((i, j), c) | (i, r) <- [0 ..] `zip` xs, (j, c) <- [0 ..] `zip` r]
  where
    rows = length xs
    cols = length $ head xs

parseRows :: String -> Grid
parseRows s = case parse grid "" s of
    Left err -> error (show err)
    Right g -> g
  where
    grid = toGrid <$> many letter `sepEndBy` newline

part :: Directions -> (String -> Bool) -> Char -> Grid -> Int
part directions predicate at grid = getSum . foldMap f $ assocs grid 
  where
    f ((x, y), c) = Sum . sum $ do
      guard $ c == at
      deltas <- directions
      let coords = bimap (+ x) (+ y) <$> deltas
      guard $ all validIndex coords
      guard $ predicate [grid ! coord | coord <- coords]
      pure 1
    validIndex = inRange (bounds grid)

part1 :: Grid -> Int
part1 = part directions (== "XMAS") 'X'
  where
    directions = [ f `zip` s | f <- all_, s <- all_, f /= zero || s /= zero ]
    pos = [0..3]; neg = [0, -1, -2, -3]; zero = [0, 0, 0, 0]
    all_ = [pos, neg, zero]

part2 :: Grid -> Int
part2 = part directions (`elem` rotations) 'A'
  where
    rotations = flip rotate "MMSS" <$> [0..3] 
    rotate = drop <> take
    directions = [[(-1, 1), (1, 1), (1, -1), (-1, -1)]]

solve :: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseRows