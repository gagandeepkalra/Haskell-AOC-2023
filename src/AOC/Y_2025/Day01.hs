module AOC.Y_2025.Day01 (solve) where

-- https://adventofcode.com/2025/day/1

import Text.Parsec ( char, digit, newline, many1, sepBy, parse )
import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Foldable (foldl')

data Direction = L | R deriving (Show, Eq)

data Rotation = Rotation Direction Int

parseRows :: String -> [Rotation]
parseRows s = case parse rows "" s of
    Left err -> error (show err)
    Right g -> g
  where
    rows = rotation `sepBy` newline
    int = read <$> many1 digit
    direction = (char 'L' >> return L) <|> (char 'R' >> return R)
    rotation = Rotation <$> direction <*> int

-- Start with 50, then rotate and move according to the rotations
-- Range 0-99
-- Count how many times you land on zero
part1 :: [Rotation] -> Int
part1 = foldl' (\(acc, pos) (Rotation dir dist) -> 
    let newPos = case dir of
            L -> (pos - dist + 100) `mod` 100
            R -> (pos + dist) `mod` 100
        newAcc = if newPos == 0 then acc + 1 else acc
    in (newAcc, newPos)) (0, 50) >>> fst

-- Similar to part1
-- But if you cross or land at zero count that.
part2 :: [Rotation] -> Int
part2 = foldl' (\(acc, pos) (Rotation dir dist) -> 
    let factor = dist `div` 100
        dist' = dist `mod` 100
    in case dir of
        L | pos == 0           -> (acc + factor, - dist' + 100)
          | pos - dist' <= 0   -> (acc + factor + 1, (pos - dist' + 100) `mod` 100)
          | otherwise          -> (acc + factor, pos - dist')
        R | pos == 0           -> (acc + factor, dist')
          | pos + dist' >= 100 -> (acc + factor + 1, pos + dist' - 100)
          | otherwise          -> (acc + factor, pos + dist')
    ) (0, 50) >>> fst

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseRows