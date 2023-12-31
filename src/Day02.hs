module Day02 (solve) where

-- https://adventofcode.com/2023/day/1

import Text.Parsec

data Game = Game { gid :: Int, draw :: Draw } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

instance Semigroup Draw where
  (Draw a b c) <> (Draw x y z) = Draw (max a x) (max b y) (max c z)

instance Monoid Draw where
  mempty = Draw 0 0 0

{-
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
-}
parseGames :: String -> [Game]
parseGames s = case parse games "" s of
    Left err -> error (show err)
    Right g -> g
  where
    games  = game `sepBy` newline
    game   = Game <$> (string "Game " *> int <* char ':') <*> draws
    int    = read <$> many1 digit
    draws  = chainl colors (char ';' >> pure (<>)) mempty
    colors = chainl color (char ',' >> pure (<>)) mempty
    color  = between space space int >>= \i ->
      Draw i 0 0 <$ string "red" <|>
      Draw 0 i 0 <$ string "green" <|>
      Draw 0 0 i <$ string "blue"

-- The Elf would first like to know which games would have been possible if the bag contained 
-- only 12 red cubes, 13 green cubes, and 14 blue cubes?
part1 :: [Game] -> Int
part1 = sum . fmap (\(Game i (Draw r g b)) -> if r <= 12 && g <= 13 && b <= 14 then i else 0)

part2 :: [Game] -> Int
part2 = sum . fmap (\(Game _ (Draw r g b)) -> r * g * b)

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where p = parseGames