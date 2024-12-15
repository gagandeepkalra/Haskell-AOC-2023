{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module AOC.Y_2023.Day05 (solve, parseFertilizer) where

import qualified Data.List as L
import Text.Parsec

--seeds: 79 14 55 13
--
--seed-to-soil map:
--50 98 2
--52 50 48
--
--soil-to-fertilizer map:
--0 15 37
--37 52 2
--39 0 15
--
--fertilizer-to-water map:
--49 53 8
--0 11 42
--42 0 7

data Fertilizer = Fertilizer {seeds :: [Integer], relations :: [Relation]}
  deriving (Show)

data Relation = Relation {name :: String, components :: [Component]}
  deriving (Show)

data Component = Component {destination :: Integer, source :: Integer, range :: Integer}
  deriving (Show)

-- Consider all of the initial seed numbers listed in the ranges on the first line of the almanac.
-- What is the lowest location number that corresponds to any of the initial seed numbers?

part2 :: Fertilizer -> Integer
part2 Fertilizer {..} = minimum $ minimumLocation relations <$> inputs seeds
  where
    translate :: [Component] -> (Integer, Integer) -> [(Integer, Integer)]
    translate components pair =
      concat $
        L.unfoldr
          ( \case
              (Just (s, e), c : cs) -> let (result, remaining) = get (s, e) c in Just (result, (remaining, cs))
              (Just (s, e), [])     -> Just ([(s, e)], (Nothing, []))
              (Nothing, _)          -> Nothing
          )
          (Just pair, components)
          
    -- The range encounters a mapping (set in increasing order) 
    -- Returns more and maybe leftover range.
    get :: (Integer, Integer) -> Component -> ([(Integer, Integer)], Maybe (Integer, Integer))
    get (s, e) Component {..} | s < x  && e < x            = ([(s, e)], Nothing)
                              | s < x  && e <= y           = ([(s, x - 1), (x', x' + e - x)], Nothing)
                              | s < x  && y < e            = ([(s, x - 1), (x', y')], Just (y + 1, e))
                              | x <= s && s <= y && e <= y = ([(x' + (s - x), y' - (y - e))], Nothing)
                              | x <= s && s <= y && y < e  = ([(x' + (s - x), y')], Just (y + 1, e))
                              | y < s                      = ([], Just (s, e))
                              | otherwise                  = error "impossible"
      where
        (x, y)   = (source, source + range - 1)
        (x', y') = (destination, destination + range - 1)

    minimumLocation :: [Relation] -> (Integer, Integer) -> Integer
    minimumLocation [] (s, _) = s
    minimumLocation (Relation {..} : rs) pair = minimum $ minimumLocation rs <$> translate components pair
    
    inputs (x : y : ls) = (x, x + y - 1) : inputs ls
    inputs _ = []

part1 :: Fertilizer -> Integer
part1 Fertilizer {..} = minimum $ seedToLocation <$> seeds
  where
    seedToLocation :: Integer -> Integer
    seedToLocation = L.foldl' (\acc f -> translate f . acc) id relations

    translate :: Relation -> Integer -> Integer
    translate Relation {..} i = maybe i get $ L.find inRange components
      where
        get Component {..} = destination + i - source
        inRange Component {..} = source <= i && i <= (source + range - 1)

parseFertilizer :: String -> Fertilizer
parseFertilizer s = case parse fertilizer "" s of
  Left err -> error (show err)
  Right g -> g
  where
    fertilizer = Fertilizer <$> (string "seeds:" *> space *> ints <* newline) <*> relations <* eof
    relations = newline *> relation `sepBy` newline
    relation = Relation <$> (word <* string " map:" <* newline) <*> components
    components = L.sortOn source <$> component `sepEndBy` newline -- sorted components
    component = Component <$> int <*> (space *> int) <*> (space *> int)
    ints = int `sepBy` char ' '
    int = read <$> many1 digit
    word = many1 $ letter <|> char '-'

solve :: String -> IO ()
solve input = putStrLn "--- Day 05 ---" >> print (p input) >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = parseFertilizer
