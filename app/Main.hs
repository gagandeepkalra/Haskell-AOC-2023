module Main (main) where

import qualified Data.Map as Map
import qualified AOC.Y_2023.Day01 as Y_2023_Day01
import qualified AOC.Y_2023.Day02 as Y_2023_Day02
import qualified AOC.Y_2023.Day03 as Y_2023_Day03
import qualified AOC.Y_2023.Day04 as Y_2023_Day04
import qualified AOC.Y_2023.Day05 as Y_2023_Day05
import qualified AOC.Y_2023.Day06 as Y_2023_Day06
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

solveSingle :: String -> String -> IO ()
solveSingle "2023" s = case Map.lookup s solutions2023 of
  Just f -> readFile (concat ["./data/2023/day", s, ".txt"]) >>= f
  Nothing -> putStrLn $ "Day not implemented: " ++ s
solveSingle _ _ = error ""

main :: IO ()
main = do
  args <- getArgs
  case args of
    [y, problem] -> solveSingle y problem
    _ -> error "invalid args"
