{-# LANGUAGE BangPatterns #-}
module AOC.Y_2025.Day03 (solve) where

-- https://adventofcode.com/2025/day/3

import Control.Arrow (Arrow(first))
import Control.Parallel.Strategies (parList, using, rdeepseq)
import Data.Char (digitToInt)
import Data.Foldable (Foldable(foldMap'))
import Data.Map.Strict as Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum, getSum))
import Data.Time as Time (diffUTCTime, getCurrentTime)
import Text.Parsec (digit, many1, newline, parse, sepBy)


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

part2Parallel :: [Joltage] -> Integer
part2Parallel js = sum $ using (fromMaybe 0 . flip calculate 12 <$> js) strat
  where
    strat = parList rdeepseq -- need Normal Form, NF instead of WHNF

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
            (withX, cache')    = first (fmap (fromIntegral x * (10 ^ (n-1)) + )) $ go xs (n - 1) cache
            (withoutX, cache'') = go xs n cache'
            result = (max <$> withX <*> withoutX) `orElse` withX `orElse` withoutX
            cacheFinal = Map.insert (i, n) result cache''
        in (result, cacheFinal)

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 03 ---"
    print (part1 $ p input)
    putStrLn "Timing part2..."
    start <- getCurrentTime
    let !result = part2 $ p input
    end <- getCurrentTime
    print result
    putStrLn $ "Time for part2: " ++ show (diffUTCTime end start)

    putStrLn "Timing part2Parallel..."
    start' <- getCurrentTime
    let !result' = part2Parallel $ p input
    end' <- getCurrentTime
    print result'
    putStrLn $ "Time for part2Parallel: " ++ show (diffUTCTime end' start')
  where
    p = parseRows
    