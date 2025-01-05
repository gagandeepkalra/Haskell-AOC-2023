{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module AOC.Y_2024.Day06 (solve) where

-- https://adventofcode.com/2024/day/6

import Data.Array.Unboxed
    ( Ix(inRange),
      (!),
      (//),
      amap,
      array,
      indices,
      IArray(bounds),
      UArray )

import Data.Containers.ListUtils ( nubOrd)
import Data.List (unfoldr, foldl')
import Text.Parsec ( parse, many, sepEndBy, newline, oneOf )

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

type Direction = (Delta, Delta)

data Delta = Negative | Zero | Positive deriving (Ord, Eq, Show)

instance Enum Delta where
    toEnum :: Int -> Delta
    toEnum (-1) = Negative
    toEnum 0 = Zero
    toEnum 1 = Positive
    toEnum n = error $ "Invalid Delta value: " ++ show n

    fromEnum :: Delta -> Int
    fromEnum Negative = -1
    fromEnum Zero = 0
    fromEnum Positive = 1

north, south, east, west :: Direction
north = (Negative, Zero)
south = (Positive, Zero)
east  = (Zero, Positive)
west  = (Zero, Negative)

turnRight :: Direction -> Direction
turnRight v
  | v == north = east
  | v == east  = south
  | v == south = west
  | v == west  = north
  | otherwise  = error "Invalid direction"

forward :: Point -> Direction -> Point
forward (x, y) (dx, dy) = (x + fromEnum dx, y + fromEnum dy)

parseInput :: String -> Grid Char
parseInput s = case parse grid "" s of
    Left err -> error (show err)
    Right g -> g
  where
    grid = mkGrid <$> many element `sepEndBy` newline
    element = oneOf ".#^"

part1 :: Grid Char -> Int
part1 grid = length . nubOrd  $ fst <$> places walls start
  where
    start = (,north) $ head [i | i <- indices grid, grid ! i == '^']
    walls = amap (=='#') grid

part2 :: Grid Char -> Int
part2 grid = foldl' (\acc p -> if check p then acc + 1 else acc) 0 candidates
  where
    start = (,north) $ head [i | i <- indices grid, grid ! i == '^']
    walls = amap (=='#') grid
    candidates = tail . nubOrd $ fst <$> places walls start
    check p = isCycle $ places (walls // [(p, True)]) start

places :: Grid Bool -> (Point, Direction) -> [(Point, Direction)]
places walls start = start : unfoldr move start
  where
    move (p, d) 
      | not (inRange (bounds walls) p') = Nothing
      | walls ! p' = move (p, turnRight d)
      | otherwise = Just ((p', d), (p', d))
      where p' = forward p d

-- Floyd's cycle-finding algorithm
isCycle :: Eq a => [a] -> Bool
isCycle l = go l l
  where
    go (x:xs) (_:y:ys) | x == y = True | otherwise = go xs ys 
    go _ _ = False

solve :: String -> IO ()
solve input = putStrLn "--- Day 06 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseInput