module AOC.Y_2025.Day02 (solve) where

-- https://adventofcode.com/2025/day/2

import Text.Parsec ( char, digit, many1, sepBy, parse )

data Range = Range Int Int

parseRows :: String -> [Range]
parseRows s = case parse ranges "" s of
    Left err -> error (show err)
    Right g -> g
  where
    ranges = range `sepBy` char ','
    int    = read <$> many1 digit
    range  = Range <$> int <* char '-' <*> int

isInvalid1 :: Int -> Bool
isInvalid1 i | odd (length s) = False
          |otherwise = take half s == drop half s
  where
    half = length s `div` 2
    s = show i

part1 :: [Range] -> Integer
part1 ranges = sum [fromIntegral x | Range start end <- ranges, x <- [start..end], isInvalid1 x]

isInvalid2 :: Int -> Bool
isInvalid2 i | l <= 1 = False
            | otherwise = or 
              [ s == repeated 
              | offset <- [1..half]
              , l `mod` offset == 0
              , let repeated = take l (cycle (take offset s))
              ]
  where
    l = length s
    half = l `div` 2
    s = show i

part2 :: [Range] -> Integer
part2 ranges = sum  [fromIntegral x | Range start end <- ranges, x <- [start..end], isInvalid2 x]

solve :: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseRows