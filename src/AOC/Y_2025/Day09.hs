{-# LANGUAGE FlexibleContexts #-}
module AOC.Y_2025.Day09 (solve) where

-- https://adventofcode.com/2025/day/9

import Control.Monad (foldM, foldM_, forM_)
import Data.Array ( Array, (!), bounds )
import Data.Array.IO (IOArray, freeze, newArray, writeArray)
import Data.Bifunctor (bimap)
import Data.Functor (($>))
import qualified Data.IntMap as Map
import Data.List (nub, sort, tails)
import Data.Tuple.Extra (dupe, swap)
import Text.Parsec (char, digit, many1, newline, parse, sepEndBy)

type Point = (Int, Int)
type Problem = [Point]

parseInputs :: String -> Problem
parseInputs s = case parse points "" s of
    Left err -> error (show err)
    Right g -> g
  where
    points = point `sepEndBy` newline
    point = fmap swap $ (,) <$> int <*> (char ',' *> int) -- (r, c) now
    int  = read <$> many1 digit

-- maximum of pair of points that form the biggest rectangle
part1 :: Problem -> Int
part1 input = maximum [area x y | (x:ys) <- tails input, y <- ys ]

area :: Num a => (a, a) -> (a, a) -> a
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

type GridIO a = IOArray Point a
type Grid a = Array Point a

forFoldM_ :: (Foldable t, Monad m) => t a -> b -> (b -> a -> m b) -> m ()
forFoldM_ xs z f = foldM_ f z xs

forFoldM :: (Foldable t, Monad m) => t a -> b -> (b -> a -> m b) -> m b
forFoldM xs z f = foldM f z xs

part2 :: Problem -> IO Int
part2 input' = do
  f <- valid_rectangle
  return . maximum
    $ [area (uncompress p1) (uncompress p2) | p1: rest <- tails input, p2 <- rest, f p1 p2]
  where
    (compress, uncompress) = coordinateCompression input'
    input = compress <$> input'
    (max_x, max_y) = bimap maximum maximum $ unzip input

    valid_rectangle :: IO (Point -> Point -> Bool)
    valid_rectangle = do
      -- fst : -1 for nothing, 0 for inflexion points (corner points), 1,2,3.. each for a unique line
      -- snd: sequence number of the point in the polygon, starts from 0
      lines_plotted <- do
        g <- newArray ((0, 0), (max_x, max_y)) (-1, -1) :: IO (GridIO (Int,Int))
        let
          -- input_chain is the list of (true) inflexion points that define the polygon corners
          input_chain = inflexionPoints input
          -- ls is the list of all polygon edges as pairs of consecutive inflexion points, including the closing edge from last to first
          ls = (last input_chain, head input_chain) : zip input_chain (tail input_chain)

        -- draw lines on the grid
        forFoldM_ ls (1, 1) $ \(acc_line, acc_idx) (p1@(x1, y1), (x2, y2)) -> do
          -- line from (x1, y1) to (x2, y2) exclusive of both points, 0 for the inflexion points
          let drawLine id0
                | x1 == x2, y1 < y2 = forFoldM [y1 + 1 .. y2 - 1]         id0 $ \idx y -> writeArray g (x1, y) (acc_line, idx) $> (idx + 1) 
                | x1 == x2, y1 > y2 = forFoldM [y1 - 1, y1 - 2 .. y2 + 1] id0 $ \idx y -> writeArray g (x1, y) (acc_line, idx) $> (idx + 1)
                | y1 == y2, x1 < x2 = forFoldM [x1 + 1 .. x2 - 1]         id0 $ \idx x -> writeArray g (x, y1) (acc_line, idx) $> (idx + 1)
                | y1 == y2, x1 > x2 = forFoldM [x1 - 1, x1 - 2 .. x2 + 1] id0 $ \idx x -> writeArray g (x, y1) (acc_line, idx) $> (idx + 1)
                | otherwise = error "Invalid line"
          writeArray g p1 (0, acc_idx)
          acc_idx' <- drawLine (acc_idx + 1)
          return (acc_line + 1, acc_idx')
        freeze g :: IO (Grid (Int, Int))

      putStrLn "Lines plotted:"
      printGrid lines_plotted (\(i, _) -> if i == -1 then '.' else '#')

      inside_regions <- do
        g <- newArray ((0, 0), (max_x, max_y)) False :: IO (GridIO Bool)
        -- ray tracing left to right
        -- state: (inside, direction), direction is 0 for invalid, 1 for when upward, -1 for downward having seen a corner
        forM_ [0..max_x] $ \x ->
          forFoldM_ [0..max_y] (False, 0 :: Int) (\acc@(inside, dir) y -> do
            let (current_value, current_idx) = lines_plotted ! (x, y)
            let current_is_inside = current_value == (-1) && inside || current_value /= (-1)
            writeArray g (x, y) current_is_inside
            let
              (prev_value, _) | y == 0 = (-1, -1)     | otherwise = lines_plotted ! (x, y - 1)
              (_, up_idx)     | x == 0 = (-1, -1)     | otherwise = lines_plotted ! (x - 1, y)  
              (_, down_idx)   | x == max_x = (-1, -1) | otherwise = lines_plotted ! (x + 1, y)  

              consequtive id1 id2 = abs (id1 - id2) == 1

              -- Determine direction of vertical edge: check if polygon extends above (x-1) or below (x+1)
              -- For ray tracing left-to-right, this tells us if the edge is going upward (1) or downward (-1)
              dir' 
                | current_idx `consequtive` up_idx   = 1     
                | current_idx `consequtive` down_idx = -1
                | otherwise = 0

              acc'
                | current_value == 0, dir == 0    = (not inside, dir')  -- first corner witnessed by the ray
                | current_value == 0, dir == dir' = (not inside, 0)     -- second corner witnessed by the ray, removed direction recording
                | current_value == 0, dir /= dir' = (inside, 0)         -- second corner witnessed by the ray, removed direction recording
                | current_value > 0, dir == 0, current_value /= prev_value = (not inside, 0) -- dir == 0, we are not inside a horizontal line
                | otherwise = acc
            return acc'
          )
        freeze g :: IO (Grid Bool)

      putStrLn "Inside regions:"
      printGrid inside_regions (\b -> if b then '#' else '.')

      -- how much right of the point is inside the region including the point
      right_projection <- do
        g <- newArray ((0, 0), (max_x, max_y)) 0 :: IO (GridIO Int)
        forM_ [0..max_x] $ \x ->
          forFoldM_ [max_y, max_y - 1 .. 0] 0 (\acc y -> do
            let current_value | inside_regions ! (x, y)  = acc + 1 | otherwise = 0
            writeArray g (x, y) current_value
            return current_value
          )
        freeze g :: IO (Grid Int)

      -- rectangle is valid if all cells inside the rectangle are inside the region
      -- for each point on the left edge, the right projection should be >= length of the rectangle
      let f (x1, y1) (x2, y2) = valid where
            breadth = abs (y2 - y1) + 1
            valid = and [right_projection ! (x, min y1 y2) >= breadth | x <- [min x1 x2..max x1 x2]]
      return f

-- chain of points, each line is either vertical or horizontal from the previous line
-- first point is connected to second, second is connected to third, etc.
-- last is connected to first.
-- reduce consecutive vertical or horizontal points to a single point
inflexionPoints :: [Point] -> [Point]
inflexionPoints [] = []
inflexionPoints [p] = [p]
inflexionPoints ps = go (last ps) ps [] where

  -- use prev to find direction, then find and commit the inflexion point in that direction
  -- p, be the first of the candidates
  go _ [] acc = acc
  go prev rest@(p:_) acc
    | prev `isHorizontal` p, (hz_points, rest') <- span (isHorizontal prev) rest, inflexion_point <- last hz_points
    = case rest' of
        (_:_) -> go inflexion_point rest' (inflexion_point: acc)
        [] | result <- reverse acc, (f:_) <- result, f `isHorizontal` inflexion_point -> result
        [] -> reverse $ inflexion_point: acc

    | prev `isVertical` p, (vt_points, rest') <- span (isVertical prev) rest, inflexion_point <- last vt_points
    = case rest' of
        (_:_) -> go inflexion_point rest' (inflexion_point: acc)
        [] | result <- reverse acc, (f:_) <- result, f `isVertical` inflexion_point -> result
        [] -> reverse $ inflexion_point: acc

    | otherwise = error "Invalid chain"

  isHorizontal (x1, _) (x2, _) = x1 == x2
  isVertical (_, y1) (_, y2) = y1 == y2

coordinateCompression :: Problem -> (Point -> Point, Point -> Point)
coordinateCompression input = (fwd, rev)
  where
    (fwd_x, rev_x) = bimap (Map.fromList . (`zip` [0..])) (Map.fromList . ([0..] `zip`)) . dupe . sort . nub . fmap fst $ input
    (fwd_y, rev_y) = bimap (Map.fromList . (`zip` [0..])) (Map.fromList . ([0..] `zip`)) . dupe . sort . nub . fmap snd $ input

    fwd (x, y) = (fwd_x Map.! x, fwd_y Map.! y)
    rev (x, y) = (rev_x Map.! x, rev_y Map.! y)

printGrid :: Grid a -> (a -> Char) -> IO ()
printGrid g f = do
  forM_ [0..rows] $ \r -> do
    forM_ [0..cols] $ \c -> do
      putChar . f $ g ! (r, c)
    putChar '\n'
  where
    (_, (rows, cols)) = bounds g

solve :: String -> IO ()
solve input = putStrLn "--- Day 09 ---" >> print (part1 p) >> (print =<< part2 p)
  where p = parseInputs input