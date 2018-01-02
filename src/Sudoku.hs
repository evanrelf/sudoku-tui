{-# LANGUAGE TemplateHaskell #-}

module Sudoku
  ( Cell(..)
  , Region
  , Grid
  , Game(..)
  , Direction(..)
  , cursor
  , grid
  , mkGame
  , moveCursor
  , answerCell
  , toggleNoteCell
  , clearCell
  , group
  , getRegion
  , demo
  ) where

import Lens.Micro
import Lens.Micro.TH (makeLenses)

data Cell
  = Given Int
  | User Int
  | Note [Int]
  | Empty
  deriving (Read, Show)

type Region = [Cell]

type Grid = [Region]

data Game = Game
  { _cursor :: (Int, Int)
  , _grid :: Grid
  }

data Direction
  = North
  | South
  | East
  | West

makeLenses ''Game

-- | Make a Game from a rows of Ints
mkGame :: [[Int]] -> Game
mkGame g = Game
  { _cursor = (4, 4)
  , _grid = map (map mkCell) g
  } where mkCell 0 = Empty
          mkCell x = Given x

-- | Move cursor a distance in a direction (wrap when reaching edges)
moveCursor :: Direction -> Int -> Game -> Game
moveCursor direction distance game@Game {_cursor = (x, y)} =
  (\c -> game & cursor .~ c) $ case direction of
    North -> (x, wrap (y - distance))
    South -> (x, wrap (y + distance))
    East  -> (wrap (x + distance), y)
    West  -> (wrap (x - distance), y)
  where wrap n | n >= 9    = n - 9
               | n < 0     = n + 9
               | otherwise = n

-- | Transform highlighted cell
transformCell :: (Cell -> Cell) -> Game -> Game
transformCell f game@Game {_cursor = (x, y)} =
  game & grid . ix y . ix x %~ f

answerCell :: Int -> Game -> Game
answerCell number game = transformCell f game
  where f (Given n) = Given n
        f _         = User number

toggleNoteCell :: Int -> Game -> Game
toggleNoteCell number game = transformCell f game
  where f (Given n) = Given n
        f (Note ns)
          | ns == [number] = Empty
          | otherwise =
            if number `elem` ns
              then Note (filter (/= number) ns)
              else Note (number : ns)
        f _ = Note [number]

clearCell :: Game -> Game
clearCell game = transformCell f game
  where f (Given n) = Given n
        f _         = Empty

-- | Group list elements into sub-lists of size n
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs
  | n > 0 = take n xs : group n (drop n xs)
  | otherwise = error "Invalid group size"

getRegion :: Int -> Grid -> [[Cell]]
getRegion number grid' =
  [[grid' !! row !! col | col <- [x..x+2]] | row <- [y..y+2]]
  where x = (number `mod` 3) * 3
        y = number - (number `mod` 3)

demo :: [[Int]]
demo = let z = 0 in
  [ [z, 6, z, z, z, z, z, 7, 3]
  , [z, 7, z, z, z, 1, 5, z, 4]
  , [z, z, z, z, 7, z, 1, z, z]
  , [7, 5, z, 8, z, 6, 4, z, z]
  , [3, z, 8, 9, 1, 5, 2, z, 7]
  , [z, z, 2, 7, z, 4, z, 5, 9]
  , [z, z, 6, z, 9, z, z, z, z]
  , [2, z, 7, 5, z, z, z, 1, z]
  , [5, 3, z, z, z, z, z, 9, z]
  ]
