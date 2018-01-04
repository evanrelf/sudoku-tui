module UI where

import Sudoku

import Brick
import Brick.Widgets.Border (border, borderWithLabel, vBorder, hBorderWithLabel)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (center, hCenter)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Flow ((|>))
import qualified Graphics.Vty as V
import Lens.Micro

(?) :: Bool -> (a, a) -> a
condition ? (true, false) = if condition then true else false

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs
  | n > 0 = take n xs : group n (drop n xs)
  | otherwise = error "Invalid group size"

styleCursor, styleCellGiven, styleCellUser, styleCellNote :: AttrName
styleCursor    = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"
styleCellUser  = attrName "styleCellUser"
styleCellNote  = attrName "styleCellNote"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (styleCursor,    bg V.brightBlack)
  , (styleCellGiven, V.defAttr)
  , (styleCellUser,  fg V.blue)
  , (styleCellNote,  fg V.yellow)
  ]

handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent game (VtyEvent (V.EvKey key [V.MCtrl])) =
  case key of
    -- Quit
    V.KChar 'c' -> halt game
    -- Undo
    V.KChar 'z' -> continue $ fromMaybe game (undo game)
    -- Reset
    V.KChar 'r' -> continue . snapshot . reset $ game
    _           -> continue game
handleEvent game (VtyEvent (V.EvKey key [V.MShift])) =
  continue $ case key of
    V.KUp    -> moveCursor North 3 game
    V.KDown  -> moveCursor South 3 game
    V.KLeft  -> moveCursor West 3 game
    V.KRight -> moveCursor East 3 game
    _        -> game
handleEvent game (VtyEvent (V.EvKey key [])) =
  continue $ case key of
    -- Move by cell
    V.KUp       -> moveCursor North 1 game
    V.KDown     -> moveCursor South 1 game
    V.KLeft     -> moveCursor West 1 game
    V.KRight    -> moveCursor East 1 game
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
    -- Erase cell
    V.KBS       -> eraseCell . snapshot $ game
    V.KChar '0' -> eraseCell . snapshot $ game
    V.KChar 'x' -> eraseCell . snapshot $ game
    -- Enter number
    V.KChar '1' -> answerCell 1 . snapshot $ game
    V.KChar '2' -> answerCell 2 . snapshot $ game
    V.KChar '3' -> answerCell 3 . snapshot $ game
    V.KChar '4' -> answerCell 4 . snapshot $ game
    V.KChar '5' -> answerCell 5 . snapshot $ game
    V.KChar '6' -> answerCell 6 . snapshot $ game
    V.KChar '7' -> answerCell 7 . snapshot $ game
    V.KChar '8' -> answerCell 8 . snapshot $ game
    V.KChar '9' -> answerCell 9 . snapshot $ game
    -- Toggle note
    V.KChar '!' -> toggleNoteCell 1 . snapshot $ game
    V.KChar '@' -> toggleNoteCell 2 . snapshot $ game
    V.KChar '#' -> toggleNoteCell 3 . snapshot $ game
    V.KChar '$' -> toggleNoteCell 4 . snapshot $ game
    V.KChar '%' -> toggleNoteCell 5 . snapshot $ game
    V.KChar '^' -> toggleNoteCell 6 . snapshot $ game
    V.KChar '&' -> toggleNoteCell 7 . snapshot $ game
    V.KChar '*' -> toggleNoteCell 8 . snapshot $ game
    V.KChar '(' -> toggleNoteCell 9 . snapshot $ game
    -- Undo
    V.KChar 'u' -> fromMaybe game (undo game)
    -- Other
    _           -> game
handleEvent game _ = continue game

highlightCursor :: Game -> [[[[Widget ()]]]] -> [[[[Widget ()]]]]
highlightCursor Game {_cursor = (x, y)} widgets =
  widgets & ix bigRow
          . ix bigCol
          . ix smallRow
          . ix smallCol
          %~ withDefAttr styleCursor
  where bigRow   = y `div` 3
        bigCol   = x `div` 3
        smallRow = y `mod` 3
        smallCol = x `mod` 3

drawGridCell :: Cell -> Widget ()
drawGridCell cell = center $ case cell of
  Given x -> withAttr styleCellGiven . str $ show x
  User x  -> withAttr styleCellUser . str $ show x
  Note xs -> map str xs'
          |> group 3
          |> map hBox
          |> vBox
          |> withAttr styleCellNote
    where xs' = map f [1..9]
          f x = (x `elem` xs) ? (show x, " ")
  Empty   -> str " "

drawGrid :: Game -> Widget ()
drawGrid game =
  map (`getRegion` game) [0..8]
  |> group 3
  |> map (map (map (map drawGridCell)))
  |> highlightCursor game
  |> map (map (map (intersperse (withBorderStyle unicode vBorder)))) -- TODO
  |> map (map (map hBox))
  |> map (map (intersperse (withBorderStyle unicode (hBorderWithLabel (str "┼───────┼"))))) -- TODO
  |> map (map vBox)
  |> map (intersperse (withBorderStyle unicodeBold vBorder))
  |> map hBox
  |> intersperse (withBorderStyle unicodeBold (hBorderWithLabel (str "╋━━━━━━━━━━━━━━━━━━━━━━━╋")))
  |> vBox
  |> border
  |> withBorderStyle unicodeBold
  -- |> setAvailableSize (55, 37)
  |> setAvailableSize (73, 37)
  |> padRight (Pad 1)

drawHelp :: Widget ()
drawHelp =
  [ "move:    ←↓↑→ / wasd / hjkl"
  , "answer:  1-9"
  , "note:    shift + 1-9"
  , "erase:   backspace / 0 / x"
  , "undo:    ctrl + z / u"
  , "reset:   ctrl + r"
  , "quit:    ctrl + c"
  ]
  |> unlines
  |> str
  |> padLeftRight 1
  |> borderWithLabel (str " Help ")
  |> withBorderStyle unicodeBold
  |> setAvailableSize (31, 12)

drawDebug :: Game -> Widget ()
drawDebug game@Game {_cursor = (x, y)} =
  [ "cursor:    (" ++ show x ++ ", " ++ show y ++ ")"
  , "region:    " ++ show (getCurrentRegion game)
  , "progress:  " ++ show (progress game)
  ]
  |> unlines
  |> str
  |> padRight Max
  |> padLeftRight 1
  |> borderWithLabel (str " Debug ")
  |> withBorderStyle unicodeBold
  |> hLimit 31

drawUI :: Game -> Widget ()
drawUI game = drawGrid game <+> (drawHelp <=> drawDebug game)

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
  -- writeFile "save.sudoku" (show $ finalGame ^. grid)
  return ()

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
