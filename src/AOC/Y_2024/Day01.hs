{-# LANGUAGE TupleSections #-}
module AOC.Y_2024.Day01 (solve) where

-- https://adventofcode.com/2024/day/1

import Data.Bifunctor (Bifunctor(bimap))
import Data.List (sort)
import Data.Map.Strict (fromListWith, lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Text.Parsec (parse, spaces, sepBy, newline, many1, digit)

import Prelude hiding (lookup)

parseTuples :: String -> [(Int, Int)]
parseTuples s = case parse tuples "" s of
    Left err -> error (show err)
    Right g -> g
  where
    tuple = (,) <$> int <*> (spaces >> int)
    tuples = tuple `sepBy` newline
    int    = read <$> many1 digit

part1 :: [(Int, Int)] -> Int
part1 = sum . fmap (abs . uncurry (-)) . uncurry zip . bimap sort sort . unzip

part2 :: [(Int, Int)] -> Int
part2 input = getSum . foldMap (\i -> Sum (i * score i)) $ f
  where
    (f, s) = unzip input
    score = fromMaybe 0 . flip lookup frequency
    frequency = fromListWith (+) (fmap (, 1) s)

solve :: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseTuples