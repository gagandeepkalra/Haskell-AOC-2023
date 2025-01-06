module AOC.Y_2024.Day07 (solve) where

-- https://adventofcode.com/2024/day/7

import Data.Foldable ( Foldable(foldl') )
import Text.Parsec ( char, digit, newline, string, many1, sepBy, parse )

type Problems = [(Integer, [Integer])]

parseInput :: String -> Problems
parseInput s = case parse rows "" s of
    Left err -> error (show err)
    Right g -> g
  where
    rows = row `sepBy` newline
    row = (,) <$> int <*> (string ": " *> ints)
    ints = int `sepBy` char ' '
    int = read <$> many1 digit

part1 :: Problems -> Integer
part1 = foldl' (\acc (target, xs) -> if equationHolds target [(+), (*)] xs then acc + target else acc) 0

part2 :: Problems -> Integer
part2 = foldl' (\acc (target, xs) -> if equationHolds target [(+), (*), cc] xs then acc + target else acc) 0
  where
    cc x y = read $ x' <> y' where x' = show x; y' = show y

equationHolds :: (Ord a, Num a) => a -> [a -> a -> a] -> [a] -> Bool
equationHolds target ops = go 0
  where
    go total [] = total == target
    go total (x:xs)
      | total > target = False
      | otherwise = any (flip go xs . ($ x) . ($ total)) ops 
  
solve :: String -> IO ()
solve input = putStrLn "--- Day 07 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseInput