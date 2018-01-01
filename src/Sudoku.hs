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

mkGame :: [[Int]] -> Game
mkGame g = Game { _cursor = (0, 0)
                  , _grid = map (map mkCell) g
                  }
  where mkCell 0 = Empty
        mkCell x = Given x

wrap :: Int -> Int -> Int -> Int
wrap lower upper val
  | val > upper = lower
  | val < lower = upper
  | otherwise = val

move :: Direction -> Game -> Game
move direction game@Game {_cursor = (x, y)} =
  case direction of
    North -> game & cursor .~ (x, wrap' (y - 1))
    South -> game & cursor .~ (x, wrap' (y + 1))
    East -> game & cursor .~ (wrap' (x + 1), y)
    West -> game & cursor .~ (wrap' (x - 1), y)
  where wrap' = wrap 0 8

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