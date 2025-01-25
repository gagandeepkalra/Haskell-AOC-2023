module AOC.Y_2024.Day13 (solve) where

-- https://adventofcode.com/2024/day/13

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Foldable (Foldable(foldMap'))
import Data.Monoid (Sum(Sum, getSum))
import Text.Parsec ( newline, parse, digit, string, many1, sepBy )

type Line = (Int, Int, Int)
type Point = (Int, Int)

intersection :: Line -> Line -> Maybe Point
intersection (a, b, c) (d, e, f) = do
  let det = a * e - b * d; num_x = c * e - b * f; num_y = a * f - c * d
  guard $ det /= 0
  guard $ num_x `mod` det == 0
  guard $ num_y `mod` det == 0
  return (num_x `div` det, num_y `div` det)

type Pair = (Line, Line)
type Input = [Pair]

-- Button A: X+94, Y+34
-- Button B: X+22, Y+67
-- Prize: X=8400, Y=5400

parseInput :: String -> Input
parseInput s = case parse input "" s of
    Left err -> error (show err)
    Right g -> g
  where
    input = problem `sepBy` (newline >> newline)
    int = read <$> many1 digit
    problem = do
      (ax, ay) <- (,) <$> (string "Button A: X+" *> int) <*> (string ", Y+" *> int <* newline)
      (bx, by) <- (,) <$> (string "Button B: X+" *> int) <*> (string ", Y+" *> int <* newline)
      (px, py) <- (,) <$> (string "Prize: X=" *> int) <*> (string ", Y=" *> int)
      return ((ax, bx, px), (ay, by, py))

part1 :: Input -> Int
part1 = getSum . foldMap' (Sum . maybe 0 (uncurry score) . uncurry intersection)
  where
    score a b | a <= 100, b <= 100 = 3 * a + b | otherwise = 0

part2 :: Input -> Int
part2 = getSum . foldMap' (Sum . maybe 0 (uncurry score) . uncurry intersection . bimap f f)
  where
    score a b = 3 * a + b
    f = second (+10000000000000)

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 13 ---"
    print $ part1 parsed
    print $ part2 parsed
  where parsed = parseInput input