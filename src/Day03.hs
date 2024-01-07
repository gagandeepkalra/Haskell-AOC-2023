{-# LANGUAGE LambdaCase, BangPatterns #-}

module Day03 (solve) where

import Data.Array
import Data.Char (isDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

type Point = (Int, Int)

data Element = Digit Char | Dot | Star | Symbol deriving Eq

parse :: String -> Array (Int, Int) Element
parse input = array ((0, 0), (rows -1, cols - 1)) [((i, j), element c) | (i, row) <- [0 ..] `zip` ls, (j, c) <- [0 ..] `zip` row]
  where
    ls = lines input
    rows = length ls
    cols = length $ head ls
    element c
      | c == '.'  = Dot
      | c == '*'  = Star
      | isDigit c = Digit c
      | otherwise = Symbol

neighbours :: Int -> (Int, Int) -> (Point, Point) -> [Point]
neighbours r (c1, c2) bound =
  filter (inRange bound) $
    [(r, c1 - 1), (r, c2 + 1)] ++ range ((r - 1, c1 - 1), (r - 1, c2 + 1)) ++ range ((r + 1, c1 - 1), (r + 1, c2 + 1))
    
digitsInRow :: Eq a => a -> ((a, b), Element) -> Bool
digitsInRow r ((rr, _), Digit _) = r == rr
digitsInRow _ _                  = False

digitsToInt :: [(a, Element)] -> Int
digitsToInt = read . mapMaybe (\case (_, Digit c) -> Just c; _ -> Nothing)

part1 :: Array (Int, Int) Element -> Int
part1 arr = flip go 0 $ assocs arr
  where
    b = bounds arr
    
    go [] acc = acc
    go ls@(((r, _), Digit _) : _) !acc =
      let (digits, rest) = flip span ls $ digitsInRow r
          c1 = snd . fst . head $ digits
          c2 = snd . fst . last $ digits
          shouldInclude = elem Symbol $ (arr !) <$> neighbours r (c1, c2) b
       in go rest $ if shouldInclude then acc + digitsToInt digits else acc
    go (_ : rest) acc = go rest acc
    
--A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying 
-- those two numbers together.
--
--This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out 
-- which gear needs to be replaced.
part2 :: Array (Int, Int) Element -> Int
part2 arr = flip go M.empty $ assocs arr
  where
    b = bounds arr
    
    go :: [((Int, Int), Element)] -> M.Map Point [Int] -> Int
    go [] acc = M.foldl' (\r ls -> case ls of [a1, a2] -> r + a1 * a2; _ -> r) 0 acc
    go ls@(((r, _), Digit _) : _) !acc =
      let (digits, rest) = flip span ls $ digitsInRow r
          n = digitsToInt digits
          stars = filter ((== Star) . (arr !)) $ neighbours r (c1, c2) b
            where c1 = snd . fst . head $ digits
                  c2 = snd . fst . last $ digits
          accU = foldl' (\acc1 p -> M.insertWith (++) p [n] acc1) acc stars
       in go rest accU
    go (_ : rest) acc = go rest acc

solve :: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parse
