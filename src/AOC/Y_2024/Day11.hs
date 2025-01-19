module AOC.Y_2024.Day11 (solve) where

-- https://adventofcode.com/2024/day/11

import Data.Foldable (Foldable(foldMap'))
import Data.Map as Map (fromListWith, toList)
import Data.Monoid (Sum(Sum, getSum))
import Text.Parsec (digit, many, parse, sepBy, space)

type Problem = [Integer]

parseInput :: String -> Problem
parseInput s = case parse problem "" s of
    Left err -> error (show err)
    Right g -> g
  where
    problem = int `sepBy` space
    int = read <$> many digit

{-
If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.

If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. 
The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved 
on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)

If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 
2024 is engraved on the new stone.
-}
rule :: (Eq a, Num a, Read a, Show a) => a -> [a]
rule 0 = [1]
rule n
  | even l = [read a, read b]
  | otherwise = [n * 2024]
  where
    s = show n
    l = length s
    (a, b) = splitAt (l `div` 2) s

applyN :: Int -> (a -> a) -> a -> a
applyN n f = snd . until ((== 0) . fst) (\(i, x) -> (i - 1, f x)) . (,) n

part :: Int -> Problem -> Int
part n = getSum . foldMap' (Sum . snd) . applyN n f . (`zip` [1, 1..])
  where
    f = Map.toList
      . Map.fromListWith (+)
      . (>>= \(x, c) -> rule x `zip` [c, c])

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 11 ---"
    print $ part 25 parsed
    print $ part 75 parsed
  where parsed = parseInput input