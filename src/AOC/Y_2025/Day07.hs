{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module AOC.Y_2025.Day07 (solve) where

-- https://adventofcode.com/2025/day/7

import Control.Monad (forM_, join)
import Control.Monad.Extra ((&&^), whenM)
import Data.Array (Array)
import Data.Array.Base (array, bounds, (!))
import Data.Array.IO (Ix(inRange), IOUArray, MArray(getBounds), newListArray, readArray, writeArray, freeze)
import Data.Foldable (find, foldlM)
import Data.Functor (($>), (<&>))
import Data.Monoid (Sum (Sum, getSum))

{- 
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
............... 
-}

type Grid a  = Array Point a -- immutable grid, Boxed array, can hold thunks
type MGrid a = IOUArray Point a -- mutable grid, Unboxed array, cannot hold thunks
type Point  = (Int, Int)

mkGrid :: MArray IOUArray e IO => [[e]] -> IO (MGrid e)
mkGrid xs = newListArray ((0, 0), (rows - 1, cols - 1)) . join $ xs
  where
    rows = length xs
    cols = length $ head xs

parseInput :: String -> IO (MGrid Char)
parseInput = mkGrid . lines

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM (\acc a -> mappend acc <$> f a) mempty

isPipeOrStart :: Char -> Bool
isPipeOrStart c = c == '|' || c == 'S'

part1 :: MGrid Char -> IO Integer
part1 grid = getSum <$> do
  (_ , (rows, cols)) <- getBounds grid
  flip foldMapM [1.. rows] $ \r -> flip foldMapM [0.. cols] $ \c -> do
    incoming <- isPipeOrStart <$> readArray grid (r - 1, c)
    readArray grid (r, c) >>= \case
      '.' | incoming  -> writeArray grid (r, c) '|'
                      $> Sum 0
      '^' | incoming  -> replaceArray (r, c - 1) '.' '|'
                      >> replaceArray (r, c + 1) '.' '|'
                      $> Sum 1
      _ -> return mempty
  where
    replaceArray coords value newVal =
      whenM (valid_coords &&^ expected_existing_value) $ writeArray grid coords newVal
      where
        valid_coords = getBounds grid <&> (`inRange` coords)
        expected_existing_value = readArray grid coords <&> (== value)

part2 :: Grid Char -> Int
part2 grid = process start_idx
  where
    start_idx = maybe (error "No start found") (0,) $ 
      find (('S' ==) . (grid !) . (0,)) [0..cols]
      
    a :: Array Point Int
    a = array b [(i, process i) | r <- [0 .. rows], c <- [0 .. cols], let i = (r, c)]

    process i@(r, c) 
      | r == rows, isPipeOrStart $ grid ! i = 1
      | r == rows = 0
      | isPipeOrStart $ grid ! i, grid ! ib == '^' = 
          (if c > 0 then a ! ibl else 0) + (if c < cols then a ! ibr else 0)
      | isPipeOrStart $ grid ! i, grid ! ib == '|' = a ! ib
      | otherwise = 0
      where ib = (r + 1, c); ibl = (r + 1, c - 1); ibr = (r + 1, c + 1)

    b@(_, (rows, cols)) = bounds grid

solve :: String -> IO ()
solve input = do
  putStrLn "--- Day 06 ---"
  p <- parseInput input
  printMGrid p
  print =<< part1 p
  putStrLn "Part 2:"
  printMGrid p
  print . part2 =<< freeze p

printMGrid :: MGrid Char -> IO ()
printMGrid grid = do
  (_, (rows, cols)) <- getBounds grid
  forM_ [0..rows] $ \r -> do
    forM_ [0..cols] $ \c -> putChar =<< readArray grid (r, c)
    putChar '\n'

