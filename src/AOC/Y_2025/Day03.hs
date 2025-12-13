module AOC.Y_2025.Day03 (solve) where

-- https://adventofcode.com/2025/day/3

import Text.Parsec ( digit, many1, sepBy, parse, newline )
import Data.Char (digitToInt)
import Data.Foldable (Foldable(foldMap'))
import Data.Monoid ( Sum(Sum, getSum) )
import Data.Map.Strict as Map ( Map, empty, lookup, insert )
import Control.Arrow (Arrow(first))
import Data.Maybe ( fromMaybe )

type Digit = Int

type Joltage = [Digit]

parseRows :: String -> [Joltage]
parseRows s = case parse joltages "" s of
    Left err -> error (show err)
    Right g -> g
  where
    joltages = joltage `sepBy` newline
    joltage  = fmap digitToInt <$> many1 digit

part1 :: [Joltage] -> Integer
part1 = getSum . foldMap' (Sum . fromMaybe 0 . flip calculate 2)

part2 :: [Joltage] -> Integer
part2 = getSum . foldMap' (Sum . fromMaybe 0 . flip calculate 12)

type IJoltage = [(Int, Int)] -- (digit, index)
type Cache = Map (Int, Int) (Maybe Integer)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y  = y

calculate :: Joltage -> Int -> Maybe Integer
calculate joltage limit = fst $ go (joltage `zip` [1..]) limit Map.empty
  where
    go :: IJoltage -> Int -> Cache -> (Maybe Integer, Cache)
    go _ 0 c = (Just 0, c)         -- limit expired, we give output
    go [] _ c = (Nothing, c)       -- no more digits
    go ((x, i) : xs) n cache
      | Just v <- Map.lookup (i, n) cache = (v, cache)  -- cache hit
      | otherwise =
        let
            -- Option 1: include this digit
            (withX, cache')    = first (fmap (fromIntegral x * (10 ^ (n-1)) + )) $ go xs (n - 1) cache
            -- Option 2: skip this digit
            (withoutX, cache'') = go xs n cache'
            result = (max <$> withX <*> withoutX) `orElse` withX `orElse` withoutX
            cacheFinal = Map.insert (i, n) result cache''
        in (result, cacheFinal)

solve :: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseRows