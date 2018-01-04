{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Sudoku
  ( Cell(..)
  , Grid
  , Game(..)
  , Direction(..)
  , cursor
  , grid
  , mkGame
  , moveCursor
  , answerCell
  , toggleNoteCell
  , eraseCell
  , snapshot
  , undo
  , reset
  , progress
  , getCurrentRegion
  , getRegion
  ) where

import Lens.Micro
import Lens.Micro.TH (makeLenses)

data Cell
  = Given Int
  | User Int
  | Note [Int]
  | Empty
  deriving (Read, Show)

type Grid = [[Cell]]

data Game = Game
  { _cursor :: (Int, Int)
  , _grid :: Grid
  , _previous :: Maybe Game
  }

data Direction
  = North
  | South
  | East
  | West

makeLenses ''Game

mkGame :: [[Int]] -> Game
mkGame g = Game
  { _cursor = (4, 4)
  , _grid = map (map mkCell) g
  , _previous = Nothing
  } where mkCell 0 = Empty
          mkCell x = Given x

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

transformCell :: (Cell -> Cell) -> Game -> Game
transformCell f game@Game {_cursor = (x, y)} =
  game & grid . ix y . ix x %~ f

answerCell :: Int -> Game -> Game
answerCell number = transformCell f
  where f (Given n) = Given n
        f _         = User number

toggleNoteCell :: Int -> Game -> Game
toggleNoteCell number = transformCell f
  where f (Given n) = Given n
        f (Note ns)
          | ns == [number] = Empty
          | otherwise =
            if number `elem` ns
              then Note (filter (/= number) ns)
              else Note (number : ns)
        f _ = Note [number]

eraseCell :: Game -> Game
eraseCell = transformCell f
  where f (Given x) = Given x
        f _         = Empty

snapshot :: Game -> Game
snapshot game = game & previous .~ Just game

undo :: Game -> Maybe Game
undo game = game ^. previous

reset :: Game -> Game
reset game = game & grid %~ map (map f)
  where f (Given x) = Given x
        f _         = Empty

progress :: Game -> Float
progress game = completed / total
  where cells       = concat $ game ^. grid
        completed   = fromIntegral $ length $ filter f cells
        total       = fromIntegral $ length cells
        f (Given _) = True
        f (User _)  = True
        f _         = False

getCurrentRegion :: Game -> Int
getCurrentRegion Game {_cursor = (x, y)} = ((y `div` 3) * 3) + (x `div` 3)

getRegion :: Int -> Game -> [[Cell]]
getRegion number game =
  [[(game ^. grid) !! row !! col | col <- [x..x+2]] | row <- [y..y+2]]
  where x = (number `mod` 3) * 3
        y = number - (number `mod` 3)
