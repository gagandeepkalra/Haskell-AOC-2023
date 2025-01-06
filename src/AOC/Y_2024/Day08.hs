module AOC.Y_2024.Day08 (solve) where

-- https://adventofcode.com/2024/day/8

import Control.Monad ( guard, join )
import Data.Array.Unboxed ( (!?), array, assocs, UArray )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Functor (($>))
import Data.List ( unfoldr )
import Data.Map ( Map, fromListWith )
import Data.Maybe ( isJust )
import Data.Set ( Set, fromList, size )
import Text.Parsec ( char, digit, letter, newline, sepEndBy, (<|>), many, parse )

type Grid a = UArray Point a
type Point  = (Int, Int)

mkGrid :: [[Char]] -> Grid Char
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
    grid = mkGrid <$> many element `sepEndBy` newline
    element = char '.' <|> letter <|> digit

part1 :: Grid Char -> Int
part1 grid = size $ foldMap antinodes antennas
  where
    antennas :: Map Char [Point]
    antennas = fromListWith (<>) [ (c, [p]) | (p, c) <- assocs grid, c /= '.']

    antinodes :: [Point] -> Set Point
    antinodes points = fromList . join $ do
        p1 <- points
        p2 <- points
        guard $ p1 /= p2
        pure $ locations p1 p2

    locations :: Point -> Point -> [Point]
    locations p1@(r1, c1) p2@(r2, c2) = filter (\p -> p /= p1 && p /= p2 && isJust (grid !? p)) points
      where
        dr = r2 - r1; dc = c2 - c1; dp = (dr, dc)
        points = [p1 `plus` dp, p1 `minus` dp, p2 `plus` dp, p2 `minus` dp]

part2 :: Grid Char -> Int
part2 grid = size $ foldMap antinodes antennas
  where
    antennas :: Map Char [Point]
    antennas = fromListWith (<>) [ (c, [p]) | (p, c) <- assocs grid, c /= '.']

    antinodes :: [Point] -> Set Point
    antinodes points = fromList . join $ do
        p1 <- points
        p2 <- points
        guard $ p1 /= p2
        pure $ locations p1 p2

    locations :: Point -> Point -> [Point]
    locations p1@(r1, c1) (r2, c2) = p1 : positives ++ negatives
      where
        dr = r2 - r1; dc = c2 - c1; dp = (dr, dc)
        positives = unfoldr (\p -> let p' = p `plus` dp in grid !? p' $> (p', p')) p1
        negatives = unfoldr (\p -> let p' = p `minus` dp in grid !? p' $> (p', p')) p1

plus, minus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus = operation (+)
minus = operation (-)

operation :: Bifunctor p => (t -> c -> d) -> (t, t) -> p c c -> p d d
operation op (x, y) = bimap (op x) (op y)

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 08 ---"
    print $ part1 parsed
    print $ part2 parsed
  where parsed = parseInput input