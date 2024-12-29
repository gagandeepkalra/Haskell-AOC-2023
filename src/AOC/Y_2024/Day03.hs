module AOC.Y_2024.Day03 (solve) where

-- https://adventofcode.com/2024/day/3

import Text.Parsec ( anyChar, char, digit, string, many1, (<|>), many, parse, try ) 
import Data.Monoid ( Sum(Sum, getSum) )
import Data.Functor (($>))
import Data.Foldable (foldl')

data Mul = Mul Integer Integer | Do | Dont
  deriving (Show)

parseRows :: String -> [Mul]
parseRows s = case parse muls "" s of
    Left err -> error (show err)
    Right g -> g
  where
    muls = many loop
    loop = mul <|> try (anyChar >> loop) -- need to not throw error if anyChar fails or loop finds nothing ahead
    mul  = pair <|>  dont <|> do_
    pair = try $ string "mul" >> Mul <$> (char '(' *> int) <*> (char ',' *> int <* char ')')
    do_  = try $ string "do()" $> Do
    dont = try $ string "don't()" $> Dont
    int  = read <$> many1 digit

part1 :: [Mul] -> Integer
part1 = getSum . foldMap (Sum . f)
  where
    f (Mul x y) = x * y
    f _ = 0

part2 :: [Mul] -> Integer
part2 = fst . foldl' f (0, True)
  where
    f (acc, True) (Mul x y) = (acc + x * y, True)
    f (acc, True) Dont = (acc, False)
    f (acc, False) Do = (acc, True)
    f acc_ _ = acc_

solve :: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseRows