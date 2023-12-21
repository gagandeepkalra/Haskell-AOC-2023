module Day01 (solve) where

-- https://adventofcode.com/2023/day/1

import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit)
import Data.List (find, isPrefixOf)

part1 :: [String] -> Int
part1 = sum . fmap (\s -> 10 * f s + (f . reverse $ s))
  where
    f :: String -> Int
    f s = maybe 0 digitToInt $ find isDigit s

part2 :: [String] -> Int
part2 = sum . fmap (\s -> 10 * f Forward s + (f Backward . reverse $ s))
  where
    f :: Mode -> String -> Int
    f _ [] = 0
    f m s@(c : cs) =
      if isDigit c
        then digitToInt c
        else maybe (f m cs) snd $ find ((`isPrefixOf` s) . fst) $ fromMode m
        
digits :: [(String, Int)]
digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] `zip` [1 ..]

reversedDigits :: [(String, Int)]
reversedDigits = first reverse <$> digits

data Mode = Forward | Backward

fromMode :: Mode -> [(String, Int)]
fromMode Forward = digits
fromMode Backward = reversedDigits

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = lines
