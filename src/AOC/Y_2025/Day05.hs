{-# LANGUAGE FlexibleContexts #-}
module AOC.Y_2025.Day05 (solve) where

-- https://adventofcode.com/2025/day/5

import Data.Foldable (Foldable(foldl'))
import Text.Parsec (parse, sepEndBy, many1, digit, char, endOfLine)
import Data.List (sort)

type Ranges = [(Integer, Integer)]
type Input = [Integer]

type Problem = (Ranges, Input)

parseInput :: String -> (Ranges, Input)  
parseInput s = case parse output "" s of
    Left err -> error (show err)
    Right g -> g
  where
    output = (,) <$> ranges <* endOfLine <*> inputs 

    ranges = range `sepEndBy` endOfLine
    inputs = int `sepEndBy` endOfLine

    range = (,) <$> int <* char '-' <*> int 
    int = read <$> many1 digit

part1 :: Problem -> Int
part1 (ranges, inputs) = sum [1 | input <- inputs, any (within input) ranges]
  where
    within x (a, b) = a <= x && x <= b

part2 :: Problem -> Integer
part2 (ranges, _) = sum [b - a + 1 | (a, b) <- disjointed]
  where 
    disjointed = foldl' f [] . sort $ ranges

    f [] a = [a]
    f acc@(a : rest) b 
      | Just ab <- merged a b = ab : rest
      | otherwise = b : acc
    
    merged :: (Ord a, Num a) => (a, a) -> (a, a) -> Maybe (a, a)
    merged f@(a1, b1) (a2, b2) 
      | a1 <= a2, a2 <= b1 + 1, b1 <= b2 = Just (a1, b2)   -- overlapping or adjacent
      | a1 <= a2, b2 <= b1 = Just f                        -- contained
      | otherwise = Nothing

solve :: String -> IO ()
solve input = putStrLn "--- Day 05 ---" >> print (part1 p) >> print (part2 p)
  where p = parseInput input
