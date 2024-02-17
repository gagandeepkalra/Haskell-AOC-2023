module Day06 (solve, parseStats) where

import Data.Ix (range)
import Data.List.NonEmpty (fromList)
import Data.Semigroup
import qualified Text.Parsec as P

--Time:      7  15   30
--Distance:  9  40  200

newtype Stats = Stats [Race] deriving (Show)

data Race = Race {time :: Integer, distance :: Integer} deriving (Show)

instance Semigroup Race where
  Race t1 d1 <> Race t2 d2 = Race (addDigit t1 t2) (addDigit d1 d2)

addDigit :: Integer -> Integer -> Integer
addDigit x y = read (show x ++ show y)

-- (T-t)*t = d
-- t*t - t*T + d = 0
-- values of t, for which equation at d = D is < 0
-- t = (T +|- sqrt (T*T - 4D))/2

--You have to figure out how many ways there are to win this single race
part2 :: Stats -> Integer
part2 (Stats races) = second - first + 1
  where
    first = ceiling $ (t - determinant) / 2
    second = floor $ (t + determinant) / 2
    determinant :: Double
    determinant = sqrt $ t * t - 4 * d
    t = fromIntegral tt
    d = fromIntegral dd
    Race tt dd = sconcat $ fromList races

--Your toy boat has a starting speed of zero millimeters per millisecond. For each whole millisecond
--you spend at the beginning of the race holding down the button, the boat's speed increases by one millimeter per millisecond.
part1 :: Stats -> Int
part1 (Stats races) = product . flip fmap races $ \(Race t d) ->
  length
    . takeWhile (valid t d)
    . dropWhile (not . valid t d)
    $ range (0, t)
  where
    valid time distance t = (time - t) * t > distance

parseStats :: String -> Stats
parseStats s = case P.parse stats "" s of
  Left err -> error (show err)
  Right g -> g
  where
    stats = do
      ts <- times
      ds <- distances <* P.eof
      return . Stats $ uncurry Race <$> ts `zip` ds
    times = P.string "Time:" *> ints
    distances = P.string "Distance:" *> ints
    ints = P.many1 $ P.between P.spaces P.spaces int
    int = read <$> P.many1 P.digit

solve :: String -> IO ()
solve input = putStrLn "--- Day 05 ---" >> print (p input) >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = parseStats
