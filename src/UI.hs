module UI where

import FileIO
import Game

import Brick
import Brick.Widgets.Border (border, borderWithLabel, hBorderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (center)
import Data.Char (digitToInt)
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Lens.Micro
import System.Directory (doesFileExist)

styleCursor, styleCellGiven, styleCellInput, styleCellNote :: AttrName
styleSolved, styleUnsolved :: AttrName
styleCursor    = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"
styleCellInput = attrName "styleCellInput"
styleCellNote  = attrName "styleCellNote"
styleSolved    = attrName "styleSolved"
styleUnsolved  = attrName "styleUnsolved"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (styleCursor    , bg V.brightBlack)
  , (styleCellGiven , V.defAttr)
  , (styleCellInput , fg V.blue)
  , (styleCellNote  , fg V.yellow)
  , (styleSolved    , fg V.green)
  , (styleUnsolved  , fg V.red)
  ]

handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent game (VtyEvent (V.EvKey key [V.MCtrl])) =
  case key of
    -- Quit
    V.KChar 'c' -> halt game
    -- Undo
    V.KChar 'z' -> continue $ fromMaybe game (previous game)
    -- Reset
    V.KChar 'r' -> continue . snapshotGame . resetGame $ game
    -- Other
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
    V.KBS       -> eraseCell . snapshotGame $ game
    V.KDel      -> eraseCell . snapshotGame $ game
    V.KChar '0' -> eraseCell . snapshotGame $ game
    V.KChar 'x' -> eraseCell . snapshotGame $ game
    -- Enter number
    V.KChar '1' -> answerCell 1 . snapshotGame $ game
    V.KChar '2' -> answerCell 2 . snapshotGame $ game
    V.KChar '3' -> answerCell 3 . snapshotGame $ game
    V.KChar '4' -> answerCell 4 . snapshotGame $ game
    V.KChar '5' -> answerCell 5 . snapshotGame $ game
    V.KChar '6' -> answerCell 6 . snapshotGame $ game
    V.KChar '7' -> answerCell 7 . snapshotGame $ game
    V.KChar '8' -> answerCell 8 . snapshotGame $ game
    V.KChar '9' -> answerCell 9 . snapshotGame $ game
    -- Toggle note
    V.KChar '!' -> toggleNoteCell 1 . snapshotGame $ game
    V.KChar '@' -> toggleNoteCell 2 . snapshotGame $ game
    V.KChar '#' -> toggleNoteCell 3 . snapshotGame $ game
    V.KChar '$' -> toggleNoteCell 4 . snapshotGame $ game
    V.KChar '%' -> toggleNoteCell 5 . snapshotGame $ game
    V.KChar '^' -> toggleNoteCell 6 . snapshotGame $ game
    V.KChar '&' -> toggleNoteCell 7 . snapshotGame $ game
    V.KChar '*' -> toggleNoteCell 8 . snapshotGame $ game
    V.KChar '(' -> toggleNoteCell 9 . snapshotGame $ game
    -- Undo
    V.KChar 'u' -> fromMaybe game (previous game)
    -- Other
    _           -> game
handleEvent game _ = continue game

highlightCursor :: Game -> [[[[Widget ()]]]] -> [[[[Widget ()]]]]
highlightCursor game widgets =
  widgets & ix bigRow
          . ix bigCol
          . ix smallRow
          . ix smallCol
          %~ withDefAttr styleCursor
  where (x, y) = cursor game
        bigRow   = y `div` 3
        bigCol   = x `div` 3
        smallRow = y `mod` 3
        smallCol = x `mod` 3

drawCell :: Cell -> Widget ()
drawCell cell = center $ case cell of
  Given x -> withAttr styleCellGiven . str $ show x
  Input x  -> withAttr styleCellInput . str $ show x
  Note xs -> fmap str xs'
          & chunksOf 3
          & fmap hBox
          & vBox
          & withAttr styleCellNote
    where xs' = fmap f [1..9]
          f x = if x `elem` xs then show x else " "
  Empty   -> str " "

drawGrid :: Game -> Widget ()
drawGrid game =
  fmap (`getRegion` game) [0..8]
  & chunksOf 3
  & fmap (fmap (fmap (fmap drawCell)))
  & highlightCursor game
  & fmap (fmap (fmap (intersperse (withBorderStyle unicode vBorder))))
  & fmap (fmap (fmap hBox))
  & fmap (fmap (intersperse (withBorderStyle unicode (hBorderWithLabel (str "┼───────┼")))))
  & fmap (fmap vBox)
  & fmap (intersperse (withBorderStyle unicodeBold vBorder))
  & fmap hBox
  & intersperse (withBorderStyle unicodeBold (hBorderWithLabel (str "╋━━━━━━━━━━━━━━━━━━━━━━━╋")))
  & vBox
  & border
  & withBorderStyle unicodeBold
  & setAvailableSize (73, 37)
  & padRight (Pad 1)

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
  & unlines
  & str
  & padLeftRight 1
  & borderWithLabel (str " Help ")
  & withBorderStyle unicodeBold
  & setAvailableSize (31, 12)

drawDebug :: Game -> Widget ()
drawDebug game =
  [ "cursor:    (" <> show x <> ", " <> show y <> ")"
  , "progress:  " <> show (gameProgress game) <> "%"
  , "solved:    " <> show (gameSolved game)
  ]
  & unlines
  & str
  & padRight Max
  & padLeftRight 1
  & borderWithLabel (str " Debug ")
  & withBorderStyle unicodeBold
  & hLimit 31
  where (x, y) = cursor game

drawSolved :: Game -> Widget ()
drawSolved game
  | completed && solved =
    str "SOLVED" & withAttr styleSolved & commonModifier
  | completed && not solved =
    str "INCORRECT" & withAttr styleUnsolved & commonModifier
  | otherwise = emptyWidget
  where
    completed = gameProgress game == 100
    solved    = gameSolved game
    commonModifier
      = setAvailableSize (31, 3)
      . withBorderStyle unicodeBold
      . border
      . center

drawUI :: Game -> Widget ()
drawUI game =
  drawGrid game <+> ( drawHelp
                <=>   drawDebug game
                <=>   drawSolved game
                    )

app :: App Game e ()
app = App
  { appDraw         = \x -> [drawUI x]
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attributes
  }

main :: IO ()
main = do
  putStr $ unlines
    [ "SUDOKU"
    , "  1) Load demo game"
    , "  2) Load file"
    , "  3) Load autosave"
    , "  4) Load game string"
    , "  *) Quit"
    ]
  response <- prompt "> "
  case head' response of
    '1' -> do
      endGame <- defaultMain app (mkGame demo)
      promptSave endGame
      saveGame "autosave.sudoku" endGame
    '2' -> do
      filename <- prompt "Filename: "
      exists <- doesFileExist filename
      if exists then do
        game <- loadGame filename
        endGame <- defaultMain app game
        promptSave endGame
        saveGame "autosave.sudoku" endGame
      else
        putStrLn $ "File '" <> filename <> "' does not exist"
    '3' -> do
      exists <- doesFileExist "autosave.sudoku"
      if exists then do
        game <- loadGame "autosave.sudoku"
        endGame <- defaultMain app game
        promptSave endGame
        saveGame "autosave.sudoku" endGame
      else
        putStrLn "File 'autosave.sudoku' does not exist"
    '4' -> do
      gameString <- prompt "Game string: "
      let game = (mkGame . fmap digitToInt) gameString
      endGame <- defaultMain app game
      promptSave endGame
      saveGame "autosave.sudoku" endGame
    _   -> putStrLn "Quitting..."
  where head' [] = ' '
        head' x  = head x

demo :: [Int]
demo = let z = 0 in
  [ z, 6, z, z, z, z, z, 7, 3
  , z, 7, z, z, z, 1, 5, z, 4
  , z, z, z, z, 7, z, 1, z, z
  , 7, 5, z, 8, z, 6, 4, z, z
  , 3, z, 8, 9, 1, 5, 2, z, 7
  , z, z, 2, 7, z, 4, z, 5, 9
  , z, z, 6, z, 9, z, z, z, z
  , 2, z, 7, 5, z, z, z, 1, z
  , 5, 3, z, z, z, z, z, 9, z
  ]
