{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module AOC.Y_2024.Day15 (solve) where

-- https://adventofcode.com/2024/day/15

import Control.Monad (mfilter, join, (>=>), foldM, foldM_, forM_)
import Data.Array.IO (IOUArray, MArray, newListArray, getAssocs, readArray, writeArray, getBounds)
import Data.Bifunctor (Bifunctor(first, second))
import Data.Foldable (Foldable(foldl', foldMap'), find)
import Data.List (unfoldr)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Data.Set as Set (Set, member, empty, insert, union, notMember, null, size)
import Text.Parsec (newline, sepEndBy, many, parse, letter, anyChar, try, oneOf, endBy)
import Data.Functor (($>), (<&>))
import Control.Arrow ((>>>))

type Grid a = IOUArray Point a
type Point  = (Int, Int)

mkGrid :: MArray IOUArray e IO => [[e]] -> IO (Grid e)
mkGrid xs = newListArray ((0, 0), (rows - 1, cols - 1)) . join $ xs
  where
    rows = length xs
    cols = length $ head xs

{-
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
-}

type Problem = (Grid Char, [Direction])

parseInput :: String -> IO Problem
parseInput s = case parse problem "" s of
    Left err -> error (show err)
    Right g -> g
  where
    problem = (\g d -> (, d) <$> g) <$> grid <*> directions
    grid = mkGrid <$> many (oneOf ".@#O") `endBy` newline
    directions = join <$> many (mkDirection <$> oneOf "><^v") `sepEndBy` newline

data Direction = N | S | E | W deriving (Show, Eq)

mkDirection :: Char -> Direction
mkDirection '^' = N
mkDirection 'v' = S
mkDirection '>' = E
mkDirection '<' = W
mkDirection _ = error "Invalid direction"

apply :: Direction -> Point -> Point
apply N = north
apply S = south
apply E = east
apply W = west

north, south, east, west :: Point -> Point
north = first (subtract 1)
south = first (+ 1)
east  = second (+ 1)
west  = second (subtract 1)

start :: Grid Char -> IO Point
start = getAssocs >=> return . fst . fromMaybe (error "No Start Found!") . find (('@' ==) . snd)

transition :: Grid Char -> Point -> Direction -> IO Point
transition grid p d = do
    let p' = apply d p
    readArray grid p' >>= \case
        '#' -> return p 
        '.' -> writeArray grid p' '@'
            >> writeArray grid p  '.'
            $> p'
        'O' -> do
            canMoveBox p' >>= \case
                Just p'' -> do
                    writeArray grid p'' 'O'
                    writeArray grid p'  '@'
                    writeArray grid p   '.'
                    return p'
                Nothing -> 
                    return p
        _ -> error "Invalid Character"
    where 
    canMoveBox p = do
        let p' = apply d p
        readArray grid p' >>= \case
            '.' -> return (Just p')
            'O' -> canMoveBox p'
            _ -> return Nothing


cellValue :: Point -> Char -> Int
cellValue (x, y) 'O' = 100 * x  + y
cellValue _ _ = 0

part1 :: Problem -> IO Int
part1 (grid, directions) = do
    startPos <- start grid
    foldM_ (transition grid) startPos directions
    assocs <- getAssocs grid
    return . getSum . foldMap (Sum . uncurry cellValue) $ assocs

part2 :: Problem -> IO Int
part2 = undefined

test1 :: String
test1 = unlines
    [ "########"
    , "#..O.O.#"
    , "##@.O..#"
    , "#...O..#"
    , "#.#.O..#"
    , "#...O..#"
    , "#......#"
    , "########"
    , ""
    , "<^^>>>vv<v>>v<<"
    ]

test2 = unlines
    [ "##########"
    , "#..O..O.O#"
    , "#......O.#"
    , "#.OO..O.O#"
    , "#..O@..O.#"
    , "#O#..O...#"
    , "#O..O..O.#"
    , "#.OO.O.OO#"
    , "#....O...#"
    , "##########"
    , ""
    , "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
    , "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
    , "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
    , "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
    , "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
    , "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
    , ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
    , "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
    , "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
    , "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
    ]

printGrid :: Grid Char -> IO ()
printGrid grid = do
    (_, (rows, cols)) <- getBounds grid
    forM_ [0..rows] $ \r -> do
        forM_ [0..cols] $ \c -> do
            v <- readArray grid (r, c)
            putChar v
        putStrLn ""

solve :: String -> IO ()
solve input = do
    putStrLn "--- Day 12 ---"
    putStrLn test1
    parsed <- parseInput test1
    part1 parsed >>= print
    printGrid (fst parsed)

    putStrLn test2
    parsed <- parseInput test2
    part1 parsed >>= print
    printGrid (fst parsed)

    parsed <- parseInput input
    part1 parsed >>= print
    -- part2 parsed >>= print