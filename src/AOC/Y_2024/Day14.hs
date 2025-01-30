module AOC.Y_2024.Day14 (solve) where

-- https://adventofcode.com/2024/day/14

import Data.Bifunctor (Bifunctor(bimap))
import Text.Parsec ( char, digit, newline, string, many1, sepBy, (<|>), parse )
import Data.Maybe (mapMaybe)
import Data.List (group, sort, minimumBy)
import Data.Function (on)

type Point = (Int, Int) -- (column, rows); (0, 0) is top-left
type Velocity = Point   -- (column, rows)

type Input = [(Point, Velocity)]

-- p=0,4 v=3,-3
-- p=6,3 v=-1,-3
-- p=10,3 v=-1,2
parseInput :: String -> Input
parseInput s = case parse input "" s of
    Left err -> error (show err)
    Right g -> g
  where
    input = problem `sepBy` newline
    problem = (,) <$> (string "p=" *> tuple <* char ' ') <*> (string "v=" *> tuple)
    tuple = (,) <$> int <* char ',' <*> int
    int = (read <$> many1 digit) <|> (negate <$> (char '-' *> int))

columns,rows :: Int
columns = 101; rows = 103

quadrant :: Point -> Maybe Int
quadrant (c, r)
  | c < mc && r < mr = Just 1
  | c > mc && r < mr = Just 2
  | c < mc && r > mr = Just 3
  | c > mc && r > mr = Just 4
  | otherwise = Nothing
  where
    mc = columns `div` 2; mr = rows `div` 2

part1 :: Input -> Int
part1 = product . fmap length . group . sort . mapMaybe (quadrant . uncurry move)
  where
    move (c, r) (vc, vr) = ((c + 100 * vc) `mod` columns, (r + 100 * vr) `mod` rows)

variance :: [Int] -> Int
variance xs = sum $ (\x -> (x - m) * (x - m)) <$> xs
  where
    m = sum xs `div` length xs

part2 :: Input -> Int
part2 input = chineseRemainder [(bX, columns), (bY, rows)]
  where
    xVectors = bimap fst fst <$> input
    yVectors = bimap snd snd <$> input

    bX = minimumBy (compare `on` (\t -> variance . fmap (\(x, v) -> (x + t*v) `mod` columns) $ xVectors)) [0..columns]
    bY = minimumBy (compare `on` (\t -> variance . fmap (\(y, v) -> (y + t*v) `mod` rows)    $ yVectors)) [0..rows]

    -- t = bX (mod columns)
    -- t = bY (mod rows)
    -- x and y are independent
    -- we find the best x and y separately, minimizing the variance of the x and y coordinates
    -- then we find a `t` that satisfies both equations


-- Chinese Remainder Theorem
-- returns the smallest x such that x = a1 (mod n1), x = a2 (mod n2), ...
chineseRemainder :: [(Int, Int)] -> Int
chineseRemainder = fst . foldl1 crt
  where
    crt (a1, n1) (a2, n2) = (a, n1 * n2)
      where
        (m1, m2, _) = extendedGCD n1 n2
        y1 = n2; y2 = n1                                  -- y1 = N / n1; y2 = N / n2
        z1 = m2; z2 = m1                                  -- z1 = (y1 ^ -1) mod n1; z2 = (y2 ^ -1) mod n2
        a = (a1 * y1 * z1 + a2 * y2 * z2) `mod` (n1 * n2) -- a mod n1 = a1 and a mod n2 = a2 verifies

-- Extended Euclidean Algorithm
-- returns (a, b, g) such that g = gcd(x, y) = a*x + b*y
extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD x 0 = (1, 0, x)
extendedGCD x y = (a', b', g)
  where
    (a, b, g) = extendedGCD y (x `mod` y)
    a' = b; b' = a - b * (x `div` y)

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 14 ---"
    print $ part1 parsed
    print $ part2 parsed
  where parsed = parseInput input

