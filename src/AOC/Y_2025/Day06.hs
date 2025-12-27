{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
module AOC.Y_2025.Day06 (solve) where

-- https://adventofcode.com/2025/day/6

import Data.Array.Base (UArray, (!), array, IArray (bounds))
import Data.Char (isDigit)
import Data.Foldable (Foldable (foldMap', foldl'))
import Data.Functor (($>))
import Data.List (groupBy, transpose)
import Data.Monoid (Sum (Sum, getSum))
import Text.Parsec (char, digit, endOfLine, many, many1, parse, sepEndBy, try, (<|>))

data Op = Add | Mul

instance Show Op where
  show :: Op -> String
  show Add = "+"
  show Mul = "*"

applyOp :: Op -> Integer -> Integer -> Integer
applyOp Add = (+)
applyOp Mul = (*)

zeroOp :: Op -> Integer
zeroOp Add = 0
zeroOp Mul = 1

-- 123 328  51 64 
--  45 64  387 23 
--   6 98  215 314
-- *   +   *   + 

type Problem1 = (Grid Int, [Op])

type Grid a = UArray Point a
type Point  = (Int, Int)

mkGrid :: IArray UArray e => [[e]] -> Grid e
mkGrid xs = array ((0, 0), (rows - 1, cols - 1))
    [((i, j), c)
    | (i, r) <- [0 ..] `zip` xs
    , (j, c) <- [0 ..] `zip` r
    ]
  where
    rows = length xs
    cols = length $ head xs

parseInput1 :: String -> Problem1
parseInput1 s = case parse output "" s of
    Left err -> error (show err)
    Right g -> g
  where
    output = (,) <$> grid <*> operations
    grid = mkGrid <$> ints `sepEndBy` endOfLine

    ints = many1 int <* spaces
    int = try $ spaces *> (read <$> many1 digit)

    operations = many1 operation <* spaces
    operation = try $ spaces *> (char '+' $> Add <|> char '*' $> Mul)

    spaces = many space
    space = char ' '

part1 :: Problem1 -> Integer
part1 (grid, ops) = sum $ extract <$> ops `zip` [0..c]
  where
    (_, (r, c)) = bounds grid
    extract (op, j) = foldl' (\acc i -> f acc (fromIntegral $ grid ! (i, j))) (zeroOp op) [0..r]
      where f = applyOp op

-- 123 328  51 64 
--  45 64  387 23 
--   6 98  215 314
-- *   +   *   + 

type Problem2 = ([[Char]], [Op])

parseInput2 :: String -> Problem2
parseInput2 s = (rows, ops)
  where
    ls = lines s
    rows = init ls
    ops = either (error . show) id $ parse operations "" (last ls)
    operations = many1 operation <* spaces
    operation = try $ spaces *> (char '+' $> Add <|> char '*' $> Mul)
    spaces = many space
    space = char ' '

part2 :: Problem2 -> Integer
part2 (rows, ops) 
  = getSum 
  . foldMap' (\(op, ls) -> Sum $ foldl' (applyOp op) (zeroOp op) ls)
  . zip ops
  . fmap (fmap mkInt)
  . splitByBlank 
  . transpose
  $ rows
  where
    splitByBlank :: [String] -> [[String]]
    splitByBlank = filter (not . isEmpty) . groupBy (\a b -> not (isEmpty [a]) && not (isEmpty [b]))

    isEmpty :: [String] -> Bool
    isEmpty = all $ all (== ' ')

    mkInt :: String -> Integer
    mkInt = read . takeWhile isDigit . dropWhile (not . isDigit)

solve :: String -> IO ()
solve input = putStrLn "--- Day 06 ---" >> print p1 >> print (part1 p1) >> print p2 >> print (part2 p2)
  where 
    p1 = parseInput1 input
    p2 = parseInput2 input
