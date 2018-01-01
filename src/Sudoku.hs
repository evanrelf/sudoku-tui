{-# LANGUAGE TemplateHaskell #-}

module Sudoku where

import Lens.Micro
import Lens.Micro.TH (makeLenses)

data Cell
  = Given Int
  | User Int
  | Empty
  | Note [Int]

type Region = [Cell]

type Grid = [Region] -- rows

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

mkGame :: [[Int]] -> Game
mkGame g = Game
  { _cursor = (0, 0)
  , _grid = map (map mkCell) g
  } where mkCell 0 = Empty
          mkCell x = Given x

move :: Direction -> Game -> Game
move direction game@Game {_cursor = (x, y)} =
  case direction of
    North -> game & cursor .~ (x, wrap (y - 1))
    South -> game & cursor .~ (x, wrap (y + 1))
    East -> game & cursor .~ (wrap (x + 1), y)
    West -> game & cursor .~ (wrap (x - 1), y)
  where wrap val
          | val > upper = lower
          | val < lower = upper
          | otherwise = val
          where lower = 0
                upper = 8

answer :: Int -> Game -> Game
answer number game@Game {_cursor = (x, y)} =
  game & grid . ix y . ix x %~ modify number
  where modify _ c@(Given _) = c
        modify 0 _ = Empty
        modify n _ = User n

toggleNote :: Int -> Game -> Game
toggleNote number game@Game {_cursor = (x, y)} =
  game & grid . ix y . ix x %~ modify number
  where modify _ g@(Given _) = g
        modify n (Note ns) = if n `elem` ns
          then Note (filter (== n) ns)
          else Note (n : ns)
        modify n _ = Note [n]

generateGrid :: IO Grid
generateGrid = undefined