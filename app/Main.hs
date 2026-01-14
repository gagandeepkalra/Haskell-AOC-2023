module Main (main) where

import qualified Data.Map as Map
import qualified AOC.Y_2023.Day01 as Y_2023_Day01
import qualified AOC.Y_2023.Day02 as Y_2023_Day02
import qualified AOC.Y_2023.Day03 as Y_2023_Day03
import qualified AOC.Y_2023.Day04 as Y_2023_Day04
import qualified AOC.Y_2023.Day05 as Y_2023_Day05
import qualified AOC.Y_2023.Day06 as Y_2023_Day06
import qualified AOC.Y_2024.Day01 as Y_2024_Day01
import qualified AOC.Y_2024.Day02 as Y_2024_Day02
import qualified AOC.Y_2024.Day03 as Y_2024_Day03
import qualified AOC.Y_2024.Day04 as Y_2024_Day04
import qualified AOC.Y_2024.Day05 as Y_2024_Day05
import qualified AOC.Y_2024.Day06 as Y_2024_Day06
import qualified AOC.Y_2024.Day07 as Y_2024_Day07
import qualified AOC.Y_2024.Day08 as Y_2024_Day08
import qualified AOC.Y_2024.Day09 as Y_2024_Day09
import qualified AOC.Y_2024.Day10 as Y_2024_Day10
import qualified AOC.Y_2024.Day11 as Y_2024_Day11
import qualified AOC.Y_2024.Day12 as Y_2024_Day12
import qualified AOC.Y_2024.Day13 as Y_2024_Day13
import qualified AOC.Y_2024.Day14 as Y_2024_Day14
import qualified AOC.Y_2024.Day15 as Y_2024_Day15
import qualified AOC.Y_2025.Day01 as Y_2025_Day01
import qualified AOC.Y_2025.Day02 as Y_2025_Day02
import qualified AOC.Y_2025.Day03 as Y_2025_Day03
import qualified AOC.Y_2025.Day04 as Y_2025_Day04
import qualified AOC.Y_2025.Day05 as Y_2025_Day05
import qualified AOC.Y_2025.Day06 as Y_2025_Day06
import qualified AOC.Y_2025.Day07 as Y_2025_Day07
import qualified AOC.Y_2025.Day08 as Y_2025_Day08
import qualified AOC.Y_2025.Day09 as Y_2025_Day09
import System.Environment ( getArgs )

solutions2023 :: Map.Map String (String -> IO ())
solutions2023 = Map.fromList
  [ ("01", Y_2023_Day01.solve)
  , ("02", Y_2023_Day02.solve)
  , ("03", Y_2023_Day03.solve)
  , ("04", Y_2023_Day04.solve)
  , ("05", Y_2023_Day05.solve)  
  , ("06", Y_2023_Day06.solve)
  ]

solutions2024 :: Map.Map String (String -> IO ())
solutions2024 = Map.fromList
  [ ("01", Y_2024_Day01.solve)
  , ("02", Y_2024_Day02.solve)
  , ("03", Y_2024_Day03.solve)
  , ("04", Y_2024_Day04.solve)
  , ("05", Y_2024_Day05.solve)
  , ("06", Y_2024_Day06.solve)
  , ("07", Y_2024_Day07.solve)  
  , ("08", Y_2024_Day08.solve)
  , ("09", Y_2024_Day09.solve)
  , ("10", Y_2024_Day10.solve)
  , ("11", Y_2024_Day11.solve)
  , ("12", Y_2024_Day12.solve)
  , ("13", Y_2024_Day13.solve)
  , ("14", Y_2024_Day14.solve)
  , ("15", Y_2024_Day15.solve)
  ]

solutions2025 :: Map.Map String (String -> IO ())
solutions2025 = Map.fromList
  [ ("01", Y_2025_Day01.solve)
  , ("02", Y_2025_Day02.solve)
  , ("03", Y_2025_Day03.solve)
  , ("04", Y_2025_Day04.solve)
  , ("05", Y_2025_Day05.solve)
  , ("06", Y_2025_Day06.solve)
  , ("07", Y_2025_Day07.solve)
  , ("08", Y_2025_Day08.solve)
  , ("09", Y_2025_Day09.solve)
  ]

solutions :: String -> Map.Map String (String -> IO ())
solutions "2023" = solutions2023
solutions "2024" = solutions2024 
solutions "2025" = solutions2025
solutions _ = error "invalid year!"

solveSingle :: String -> String -> IO ()
solveSingle year s = case Map.lookup s (solutions year) of
  Just f -> readFile (concat ["./Data/Y_", year, "/day", s, ".txt"]) >>= f
  Nothing -> putStrLn $ "Day not implemented: " ++ s

main :: IO ()
main = do
  args <- getArgs
  case args of
    [y, problem] -> solveSingle y problem
    _ -> error "invalid args"
