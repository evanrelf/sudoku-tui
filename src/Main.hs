module Main where

data Cell
  = GivenCell Int
  | FilledCell Int
  | EmptyCell
  | Note [Int]
  deriving (Eq)

-- TODO: Should this be `[Cell]` or `Box [Cell] | Row [Cell] | Column [Cell]`?
type Region = [Cell]

type Grid = [Region]

data InputMode
  = GridMode
  | NoteMode

data Direction
  = North
  | South
  | East
  | West

data Command
  = ChangeCell Int
  | ChangeNote Int
  | Move Direction
  | SwitchMode InputMode

data CursorPosition =
  CursorPosition InputMode
                 (Int, Int)

data State =
  State Grid
        CursorPosition

-- TODO: This is an ugly method of doing this
validRegion :: Region -> Maybe Bool
validRegion region
  | EmptyCell `elem` region = Nothing
  | not $ all notNote region = Nothing
  | otherwise = undefined
  where
    notNote cell =
      case cell of
        Note _ -> False
        _ -> True

-- TODO: This is an ugly method of doing this
moveCursor :: CursorPosition -> Direction -> CursorPosition
moveCursor (CursorPosition mode (col, row)) direction =
  case direction of
    North ->
      if row == 0
        then CursorPosition mode (col, max)
        else CursorPosition mode (col, row - 1)
    South ->
      if row == max
        then CursorPosition mode (col, 0)
        else CursorPosition mode (col, row + 1)
    East ->
      if col == max
        then CursorPosition mode (0, row)
        else CursorPosition mode (col + 1, row)
    West ->
      if col == 0
        then CursorPosition mode (max, row)
        else CursorPosition mode (col - 1, row)
  where
    max =
      case mode of
        GridMode -> 9 - 1
        NoteMode -> 3 - 1

update :: State -> Command -> State
update state command =
  case command of
    ChangeCell value -> undefined
    ChangeNote value -> undefined
    Move direction -> State grid (moveCursor position direction)
    SwitchMode mode -> State grid (CursorPosition mode (col, row))
  where
    grid = (\(State x _) -> x) state
    position = (\(State _ x) -> x) state
    col = (\(CursorPosition _ (x, _)) -> x) position
    row = (\(CursorPosition _ (_, x)) -> x) position

render :: State -> IO ()
render (State grid position) = undefined

main :: IO ()
main = putStrLn "hello world"
