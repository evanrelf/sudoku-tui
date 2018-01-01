module UI where

import Sudoku

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicode)
-- import Brick.Widgets.Center (hCenter)
import Data.List (intersperse)
import Flow ((|>))
import qualified Graphics.Vty as V
import Lens.Micro

styleGiven, styleUser, styleNote, styleCursor :: AttrName
styleGiven = attrName "styleGiven"
styleUser = attrName "styleUser"
styleNote = attrName "styleNote"
styleCursor = attrName "styleCursor"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (styleGiven, fg V.white)
  , (styleUser, fg V.brightBlue)
  , (styleNote, fg V.yellow)
  , (styleCursor, bg V.brightBlack)
  ]

app :: App Game e ()
app = App { appDraw = \x -> [drawUI x]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attributes
          }

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

hl :: Game -> [[Widget ()]] -> [[Widget ()]]
hl Game {_cursor = (x, y)} ws = ws & ix y . ix x %~ withAttr styleCursor

drawCell :: Cell -> Widget ()
drawCell cell = padLeftRight 1 . padAll 1 $ case cell of
  Given x -> withAttr styleGiven . str . show $ x
  User x -> withAttr styleUser . str . show $ x
  Empty -> str " "
  Note _ -> withAttr styleNote . str $ "n"

drawNotes :: Game -> Widget ()
drawNotes _ = str "\n  1  2  3  \n\n  4  5  6  \n\n  7  8  9  \n\n"
  |> withBorderStyle unicode
  |> borderWithLabel (str " Notes ")

drawGrid :: Game -> Widget ()
drawGrid game = (game ^. grid)
  |> map (map drawCell)
  |> hl game
  |> map hBox
  -- |> intersperse hBorder
  |> vBox
  |> withBorderStyle unicode
  |> borderWithLabel (str " Sudoku ")
  -- |> hLimit 47

drawUI :: Game -> Widget ()
drawUI game = drawGrid game <+> drawNotes game

handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent s (VtyEvent (V.EvKey key [])) = case key of
  -- Quit
  V.KChar 'q' -> halt s
  V.KEsc -> halt s
  -- Reset
  V.KBS -> continue $ mkGame demo
  -- Move
  V.KUp -> continue $ move North s
  V.KDown -> continue $ move South s
  V.KLeft -> continue $ move West s
  V.KRight -> continue $ move East s
  V.KChar 'k' -> continue $ move North s
  V.KChar 'j' -> continue $ move South s
  V.KChar 'h' -> continue $ move West s
  V.KChar 'l' -> continue $ move East s
  V.KChar 'w' -> continue $ move North s
  V.KChar 's' -> continue $ move South s
  V.KChar 'a' -> continue $ move West s
  V.KChar 'd' -> continue $ move East s
  -- Number
  V.KChar '0' -> continue $ answer 0 s
  V.KChar '1' -> continue $ answer 1 s
  V.KChar '2' -> continue $ answer 2 s
  V.KChar '3' -> continue $ answer 3 s
  V.KChar '4' -> continue $ answer 4 s
  V.KChar '5' -> continue $ answer 5 s
  V.KChar '6' -> continue $ answer 6 s
  V.KChar '7' -> continue $ answer 7 s
  V.KChar '8' -> continue $ answer 8 s
  V.KChar '9' -> continue $ answer 9 s
  -- Note
  V.KChar '!' -> continue $ toggleNote 1 s
  V.KChar '@' -> continue $ toggleNote 2 s
  V.KChar '#' -> continue $ toggleNote 3 s
  V.KChar '$' -> continue $ toggleNote 4 s
  V.KChar '%' -> continue $ toggleNote 5 s
  V.KChar '^' -> continue $ toggleNote 6 s
  V.KChar '&' -> continue $ toggleNote 7 s
  V.KChar '*' -> continue $ toggleNote 8 s
  V.KChar '(' -> continue $ toggleNote 9 s
  -- Other
  _ -> continue s
handleEvent s _ = continue s

main :: IO ()
main = do
  _ <- defaultMain app (mkGame demo)
  return ()
