{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module AOC.Y_2024.Day09 (solve) where

-- https://adventofcode.com/2024/day/9

import Data.Bifunctor ( Bifunctor(second) )
import Data.Char ( digitToInt )
import Data.Either (partitionEithers)
import Data.List as List ( unfoldr, foldl' ) 
import Data.Maybe ( maybeToList )
import qualified Data.IntMap as Map
import qualified Data.Set as Set
import Text.Parsec ( digit, many, parse )

type Problem = [Int]

parseInput :: String -> Problem
parseInput s = case parse problem "" s of
    Left err -> error (show err)
    Right g -> g
  where
    problem = many $ digitToInt <$> digit

part1 :: Problem -> Integer
part1 input = go (0 :: Integer) disk available
  where
    disk = [0..] `zip` generate input
    available = reverse . filter (not . isEmpty . snd) $ disk
    crossed i j = j <= i
    isEmpty = (== -1)

    go :: Integer -> [(Int, Int)] -> [(Int, Int)] -> Integer
    go acc ((i, x) : xs) rev@((j, y) : ys)
      | i `crossed` j, isEmpty x = acc
      | i `crossed` j = acc +  toInteger (i * x)
      | isEmpty x = go (acc +  toInteger (i * y)) xs ys
      | otherwise = go (acc +  toInteger (i * x)) xs rev
    go acc _ _ = acc

    generate = concat . unfoldr f . (0,)
      where
        f (_, []) = Nothing
        f (i, [fc]) = Just (replicate fc i, (i + 1, []))
        f (i, fc : fsc : rest) = Just (replicate fc i ++ replicate fsc (-1), (i + 1, rest))

-- This time, attempt to move whole files to the leftmost span of free space blocks that could fit the file. 
-- Attempt to move each file exactly once in order of decreasing file ID number starting with the file with 
-- the highest file ID number. If there is no span of free space to the left of a file that is large enough 
-- to fit the file, the file does not move.

type State = Map.IntMap (Set.Set Int) -- Count -> Indices

addToState :: Int -> Int -> State -> State
addToState count index = Map.insertWith Set.union count (Set.singleton index)

extractGE :: Int -> Int -> State -> Maybe ((Int, Int), State) -- (count, index)
extractGE from before state = result
  where
    state' = Map.map (Set.takeWhileAntitone (< before)) state
    allPairs = 
      [ (index, count) 
      | (count, set) <- Map.assocs state'
      , from <= count
      , index <- maybeToList $ Set.lookupMin set 
      ]
    result | null allPairs = Nothing
           | otherwise = 
            let (index, count) = minimum allPairs 
            in Just ((count, index), Map.adjust (Set.delete index) count state')

part2 :: Problem -> Integer
part2 input = fst $ foldl' apply (0, initial) . reverse $ files
  where
    disk = generate input
    (files, free) = partitionEithers disk
    initial = Map.fromListWith (<>) . fmap (second Set.singleton) $ free

    apply (acc, state) (lb, c, i)
      | Just ((c', i'), state') <- found, c' > c = (acc + checkSum i', addToState (c' - c) (i' + c) state')
      | Just ((_, i'), state') <- found = (acc + checkSum i', state') -- c' == c
      | otherwise = ( acc + checkSum i, state)
      where
        found = extractGE c i state 
        checkSum idx = toInteger . sum $ (* lb) <$> [idx.. idx + c - 1]

    generate = concat . unfoldr go . (0, 0,)
      where 
        go (_, _, []) = Nothing
        go (i, lb, [fc]) = Just ([Left (lb, fc, i)], (i + fc, lb + 1, []))
        go (i, lb, fc : fsc : rest) = Just ([Left (lb, fc, i), Right (fsc, i + fc)], (i + fc + fsc, lb + 1, rest))    

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 09 ---"
    print $ part1 parsed
    print $ part2 parsed
  where parsed = parseInput input