{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module AOC.Y_2023.Day04 (solve, parseCards) where

import Control.Arrow (second)
import qualified Data.List as L
import Data.Set as S
import Text.Parsec

--you have to figure out which of the numbers you have appear in the list of winning numbers.
--The first match makes the card worth one point and each match after the first doubles
--the point value of that card.
--
--For example:
--
--Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
--Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
--Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
data Card = Card {idx :: Int, winning :: S.Set Int, numbers :: [Int]} deriving (Show)

part1 :: [Card] -> Int
part1 = sum . fmap f
  where
    f Card {..} = L.foldl' (\acc a -> if a `member` winning then increment acc else acc) 0 numbers
    increment 0 = 1
    increment acc = acc * 2

--Specifically, you win copies of the scratchcards below the winning card equal to the number of matches.
--So, if card 10 were to have 5 matching numbers, you would win one copy each of cards 11, 12, 13, 14, and 15.

part2 :: [Card] -> Int
part2 = flip go 0 . fmap (,1)
  where
    go [] acc = acc
    go ((c, factor) : cs) !acc = go (upgraded ++ notUpgrade) $ acc + factor
      where
        upgraded = second (+ factor) <$> upgrade
        (upgrade, notUpgrade) = L.splitAt (score c) cs

    score Card {..} = length . L.filter (`member` winning) $ numbers

parseCards :: String -> [Card]
parseCards s = case parse cards "" s of
  Left err -> error (show err)
  Right g -> g
  where
    cards = many1 card <* eof
    card  = Card <$> (string "Card" *> spaces *> int <* char ':') <*> wins <*> (line *> ints)
    wins  = S.fromList <$> ints
    ints  = many1 $ between spaces spaces int
    line  = char '|'
    int   = read <$> many1 digit

solve :: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = parseCards
