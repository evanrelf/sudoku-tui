module Game
  ( Cell(..)
  , Row
  , Grid
  , Game(..)
  , Direction(..)
  , mkGame
  , moveCursor
  , answerCell
  , toggleNoteCell
  , eraseCell
  , snapshotGame
  , undoGame
  , resetGame
  , gameProgress
  , getRegion
  ) where

import Data.Function ((&))
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))

data Cell
  = Given Int
  | Input Int
  | Note [Int]
  | Empty
  deriving (Read, Show)

type Row = [Cell]

type Grid = [Row]

data Game = Game
  { cursor :: (Int, Int)
  , grid :: Grid
  , previous :: Maybe Game
  } deriving (Read, Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Read, Show)

mkGame :: [Int] -> Game
mkGame ns = Game
  { cursor = (4, 4)
  , grid = ns & map mkCell & chunksOf 9
  , previous = Nothing
  } where mkCell 0 = Empty
          mkCell n = Given n

moveCursor :: Direction -> Int -> Game -> Game
moveCursor direction distance game@Game { cursor = (x, y) } =
  (\c -> game { cursor = c }) $ case direction of
    North -> (x, wrap (y - distance))
    South -> (x, wrap (y + distance))
    East  -> (wrap (x + distance), y)
    West  -> (wrap (x - distance), y)
  where wrap n | n >= 9    = n - 9
               | n < 0     = n + 9
               | otherwise = n

-- TODO: Remove need for lenses
transformCell :: (Cell -> Cell) -> Game -> Game
transformCell f game@Game { cursor = (x, y) } =
  game { grid = grid game & ix y . ix x %~ f }

answerCell :: Int -> Game -> Game
answerCell number = transformCell f
  where f (Given n) = Given n
        f _         = Input number

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

snapshotGame :: Game -> Game
snapshotGame game = game { previous = Just game }

undoGame :: Game -> Maybe Game
undoGame = previous

resetGame :: Game -> Game
resetGame game@Game { grid = g } = game { grid = map (map f) g }
  where f (Given x) = Given x
        f _         = Empty

gameProgress :: Game -> Float
gameProgress game = completed / total
  where cells       = concat $ grid game
        completed   = fromIntegral $ length $ filter f cells
        total       = fromIntegral $ length cells
        f (Given _) = True
        f (Input _) = True
        f _         = False

getRegion :: Int -> Game -> [[Cell]]
getRegion number game =
  [[grid game !! row !! col | col <- [x..x+2]] | row <- [y..y+2]]
  where x = (number `mod` 3) * 3
        y = number - (number `mod` 3)
