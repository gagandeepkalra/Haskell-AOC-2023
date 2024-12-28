module AOC.Y_2024.Day02 (solve) where

-- https://adventofcode.com/2024/day/2

import Text.Parsec ( char, digit, newline, many1, sepBy, parse )
import Data.Monoid ( Sum(Sum, getSum) )

type Row = [Int]

parseRows :: String -> [Row]
parseRows s = case parse rows "" s of
    Left err -> error (show err)
    Right g -> g
  where
    rows = ints `sepBy` newline
    ints = int `sepBy` char ' '
    int  = read <$> many1 digit

part1 :: [Row] -> Int
part1 = getSum . foldMap (\r -> Sum $ if checkSafe 0 r then 1 else 0)

checkSafe :: Int -> Row -> Bool
checkSafe n r = checkOrder increasing n Nothing Nothing r 
             || checkOrder decreasing n Nothing Nothing r

part2 :: [Row] -> Int
part2 = getSum . foldMap (\r -> Sum $ if checkSafe 1 r then 1 else 0)

decreasing :: (Ord a, Num a) => a -> a -> Bool
decreasing a b = 1 <= x && x <= 3 where x = a - b

increasing :: (Ord a, Num a) => a -> a -> Bool
increasing a b = decreasing b a

-- skips prev2 prev1 list
checkOrder :: (Int -> Int -> Bool) -> Int -> Maybe Int -> Maybe Int -> Row -> Bool
checkOrder _ _ _ _ [] = True
checkOrder _ _ (Just _) Nothing _ = False
checkOrder f n Nothing Nothing (x : xs) = checkOrder f n Nothing (Just x) xs 
checkOrder f 0 _ (Just p1) (x : xs) 
  | f p1 x = checkOrder f 0 (Just p1) (Just x) xs 
  | otherwise = False -- or break
checkOrder f n Nothing (Just p1) (x : xs) 
  | f p1 x = checkOrder f n (Just p1) (Just x) xs 
  | otherwise = checkOrder f (n-1) Nothing (Just x) xs || checkOrder f (n-1) Nothing (Just p1) xs -- keep x or keep p1
checkOrder f n (Just p2) (Just p1) (x : xs) 
  | f p1 x = checkOrder f n (Just p1) (Just x) xs 
  | f p2 x = checkOrder f (n-1) (Just p2) (Just x) xs || checkOrder f (n-1) (Just p2) (Just p1) xs -- skip p1 or skip x
  | otherwise = checkOrder f (n-1) (Just p2) (Just p1) xs -- skip x

solve :: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseRows