module UI where

import Sudoku

import Brick
import Brick.Widgets.Border (borderWithLabel, vBorder, hBorder)
import Brick.Widgets.Border.Style (unicode)
import Data.List (sort, intersperse)
import Flow ((|>))
import qualified Graphics.Vty as V
import Lens.Micro

styleCursor, styleCellGiven, styleCellUser, styleCellNote :: AttrName
styleCursor    = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"
styleCellUser  = attrName "styleCellUser"
styleCellNote  = attrName "styleCellNote"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (styleCursor,    bg V.brightBlack)
  , (styleCellGiven, fg V.brightWhite)
  , (styleCellUser,  fg V.brightBlue)
  , (styleCellNote,  fg V.yellow)
  ]

handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent game (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt game
handleEvent game (VtyEvent (V.EvKey key modifiers)) =
  continue $ case key of
    -- Move by cell
    V.KUp       -> moveCursor North distance game
    V.KDown     -> moveCursor South distance game
    V.KLeft     -> moveCursor West distance game
    V.KRight    -> moveCursor East distance game
    V.KChar 'k' -> moveCursor North 1 game
    V.KChar 'j' -> moveCursor South 1 game
    V.KChar 'h' -> moveCursor West 1 game
    V.KChar 'l' -> moveCursor East 1 game
    V.KChar 'w' -> moveCursor North 1 game
    V.KChar 's' -> moveCursor South 1 game
    V.KChar 'a' -> moveCursor West 1 game
    V.KChar 'd' -> moveCursor East 1 game
    -- Move by region
    V.KChar 'K' -> moveCursor North 3 game
    V.KChar 'J' -> moveCursor South 3 game
    V.KChar 'H' -> moveCursor West 3 game
    V.KChar 'L' -> moveCursor East 3 game
    V.KChar 'W' -> moveCursor North 3 game
    V.KChar 'S' -> moveCursor South 3 game
    V.KChar 'A' -> moveCursor West 3 game
    V.KChar 'D' -> moveCursor East 3 game
    -- Clear cell
    V.KBS       -> clearCell game
    V.KChar '0' -> clearCell game
    -- Enter number
    V.KChar '1' -> answerCell 1 game
    V.KChar '2' -> answerCell 2 game
    V.KChar '3' -> answerCell 3 game
    V.KChar '4' -> answerCell 4 game
    V.KChar '5' -> answerCell 5 game
    V.KChar '6' -> answerCell 6 game
    V.KChar '7' -> answerCell 7 game
    V.KChar '8' -> answerCell 8 game
    V.KChar '9' -> answerCell 9 game
    -- Toggle note
    V.KChar '!' -> toggleNoteCell 1 game
    V.KChar '@' -> toggleNoteCell 2 game
    V.KChar '#' -> toggleNoteCell 3 game
    V.KChar '$' -> toggleNoteCell 4 game
    V.KChar '%' -> toggleNoteCell 5 game
    V.KChar '^' -> toggleNoteCell 6 game
    V.KChar '&' -> toggleNoteCell 7 game
    V.KChar '*' -> toggleNoteCell 8 game
    V.KChar '(' -> toggleNoteCell 9 game
    -- Other
    _           -> game
  where distance = if V.MShift `elem` modifiers then 3 else 1
handleEvent game _ = continue game

-- TODO
highlightCursor :: Game -> [[[[Widget ()]]]] -> [[[[Widget ()]]]]
highlightCursor Game {_cursor = (x, y)} ws =
  ws & ix bigRow . ix bigCol . ix smallRow . ix smallCol %~ withAttr styleCursor
  where bigRow = y `div` 3
        bigCol = x `div` 3
        smallRow = y `mod` 3
        smallCol = x `mod` 3

drawCell :: Cell -> Widget ()
drawCell cell = padLeftRight 1 . padAll 1 $ case cell of
  Given x -> withAttr styleCellGiven . str $ show x
  User x  -> withAttr styleCellUser . str $ show x
  Note _  -> withAttr styleCellNote . str $ "n"
  Empty   -> str " "

-- TODO
drawGrid :: Game -> Widget ()
drawGrid game = let g = game ^. grid in map (`getRegion` g) [0..8]
  |> group 3
  |> map (map (map (map drawCell)))
  |> highlightCursor game
  |> map (map (map hBox))
  |> map (map vBox)
  |> map (intersperse vBorder)
  |> map hBox
  |> intersperse hBorder
  |> vBox
  |> withBorderStyle unicode
  |> borderWithLabel (str " Sudoku ")
  |> hLimit 49
  |> vLimit 31

-- TODO
drawNotes :: Game -> Widget ()
drawNotes Game {_cursor = (x, y), _grid = g} = getRegion r g
  |> map (map convert)
  |> map (intersperse vBorder)
  |> map hBox
  |> intersperse hBorder
  |> vBox
  |> withBorderStyle unicode
  |> borderWithLabel (str " Notes ")
  |> hLimit 31
  |> vLimit 19
  where r = ((y `div` 3) * 3) + (x `div` 3)
        convert Empty = " "
          |> str
          |> padAll 2
          |> padLeftRight 2
        convert (Note ns) = ns
          |> sort
          |> show
          |> strWrap
          |> padAll 1
          |> withAttr styleCellNote
          |> hLimit 9
        convert (Given n) = show n
          |> str
          |> padAll 2
          |> padLeftRight 2
          |> withAttr styleCellGiven
        convert (User n) = show n
          |> str
          |> padAll 2
          |> padLeftRight 2
          |> withAttr styleCellUser

drawHelp :: Widget ()
drawHelp = unlines
  [ "move:    arrows / wasd / hjkl"
  , "answer:  1-9"
  , "note:    shift + 1-9"
  , "erase:   0 / backspace"
  , "quit:    ctrl + c"
  ]
  |> str
  |> padLeftRight 1
  |> withBorderStyle unicode
  |> borderWithLabel (str " Help ")

drawDebug :: Game -> Widget ()
drawDebug Game {_cursor = (x, y)} = unlines
  [ "cursor: (" ++ show x ++ ", " ++ show y ++ ")"
  , "region: " ++ show region
  ]
  |> str
  |> padLeftRight 1
  |> withBorderStyle unicode
  |> borderWithLabel (str " Debug ")
  where region = ((y `div` 3) * 3) + (x `div` 3)

drawUI :: Game -> Widget ()
drawUI game = drawGrid game <+> (drawNotes game <=> drawHelp <=> drawDebug game)

app :: App Game e ()
app = App
  { appDraw = \x -> [drawUI x]
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const attributes
  }

main :: IO ()
main = do
  finalGame <- defaultMain app (mkGame demo)
  writeFile "save.sudoku" (show $ finalGame ^. grid)
  return ()
