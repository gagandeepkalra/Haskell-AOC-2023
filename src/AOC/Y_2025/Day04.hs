{-# LANGUAGE FlexibleContexts #-}
module AOC.Y_2025.Day04 (solve) where

-- https://adventofcode.com/2025/day/4

import Data.Array.Base
    ( UArray, (!), (!?), array, indices, IArray )
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (Foldable(foldl'))
import Data.Maybe (isJust)
import Data.Monoid ( Sum(Sum, getSum) )
import Data.Set (Set, empty, insert, member, notMember, size)
import Text.Parsec (many, newline, oneOf, parse, sepEndBy)

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

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (x, y) = bimap (+ x) (+ y)

deltas :: [Point]
deltas = [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= 0 || b /= 0]

parseRows :: String -> Grid Char
parseRows s = case parse grid "" s of
    Left err -> error (show err)
    Right g -> g
  where
    grid = mkGrid <$> many element `sepEndBy` newline
    element = oneOf ".@"

part1 :: Grid Char -> Int
part1 grid = getSum . foldMap (Sum . fromEnum . check) $ indices grid
  where 
    check p = grid ! p == '@' && length [p' | p' <- neighbours p, grid ! p' == '@'] < 4 -- fewer than 4 @ neighbours
    neighbours p = [p' | d <- deltas, let p' = p `plus` d, isJust (grid !? p')]

part2 :: Grid Char -> Int
part2 grid = size $ foldl' go empty initial
  where
    go :: Set Point -> Point -> Set Point
    go deleted p
      | p `member` deleted = deleted
      | not $ isCandidate p deleted = deleted
      | otherwise = foldl' go (insert p deleted) (neighbours p)
    
    initial = [p | p <- indices grid, isCandidate p empty]
    
    isCandidate p deleted = grid ! p == '@' && sum [1 :: Int | p' <- neighbours p, grid ! p' == '@', p' `notMember` deleted] < 4 -- fewer than 4 @ neighbours
    neighbours p = [p' | d <- deltas, let p' = p `plus` d, isJust (grid !? p')]

solve :: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseRows
