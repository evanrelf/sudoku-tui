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
import Data.List (nub)
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
  , progress :: Int
  , solved :: Bool
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
            , progress = 0
            , solved = False
            }
            & updateAll
  where mkCell 0 = Empty
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
answerCell number game = transformCell f game & updateAll
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
eraseCell game = transformCell f game & updateAll
  where f (Given x) = Given x
        f _         = Empty

snapshotGame :: Game -> Game
snapshotGame game = game { previous = Just game }

undoGame :: Game -> Maybe Game
undoGame = previous

resetGame :: Game -> Game
resetGame game@Game { grid = g } = game { grid = map (map f) g }
                                 & updateAll
  where f (Given x) = Given x
        f _         = Empty

gameProgress :: Game -> Int
gameProgress game = round (fraction * 100)
  where cells       = concat $ grid game
        completed   = fromIntegral $ length $ filter hasValue cells
        total       = fromIntegral $ length cells
        fraction    = completed / total :: Float
        hasValue (Given _) = True
        hasValue (Input _) = True
        hasValue _         = False

updateProgress :: Game -> Game
updateProgress game = game { progress = gameProgress game }

updateSolved :: Game -> Game
updateSolved game =
  if progress game < 100
  then game { solved = False }
  else game { solved = gameSolved game }

updateAll :: Game -> Game
updateAll game = game & updateProgress & updateSolved

gameSolved :: Game -> Bool
gameSolved game = rowsSolved && columnsSolved && regionsSolved
  where rowsSolved = isSolved $ getRows game
        columnsSolved = isSolved $ getColumns game
        regionsSolved = isSolved $ getRegionsFlat game
        noDuplicates l = nub l == l 
        isSolved = all noDuplicates . map (map getNum)
        getNum :: Cell -> Int
        getNum (Given i) = i
        getNum (Input i) = i
        getNum _ = error "Extracting number from cell"

getRows :: Game -> [[Cell]]
getRows = grid

getColumn :: Int -> Game -> [Cell]
getColumn column game =
  [grid game !! row !! column | row <- [0..8]]

getColumns :: Game -> [[Cell]]
getColumns game = [getColumn n game | n <- [0..8]]

getRegion :: Int -> Game -> [[Cell]]
getRegion number game =
  [[grid game !! row !! col | col <- [x..x+2]] | row <- [y..y+2]]
  where x = (number `mod` 3) * 3
        y = number - (number `mod` 3)

getRegionFlat :: Int -> Game -> [Cell]
getRegionFlat number game = concat $ getRegion number game

getRegionsFlat :: Game -> [[Cell]]
getRegionsFlat game = [getRegionFlat n game | n <- [0..8]]
