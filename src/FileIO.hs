module FileIO
  ( loadGame
  , saveGame
  , prompt
  , promptSave
  ) where

import Game

import System.IO (hFlush, stdout)

exportGame :: Game -> String
exportGame = show . grid

importGame :: String -> Game
importGame = (\g -> Game (4, 4) g Nothing) . read

loadGame :: FilePath -> IO Game
loadGame filename = importGame <$> readFile filename

saveGame :: FilePath -> Game -> IO ()
saveGame filename game = writeFile filename (exportGame game)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

promptSave :: Game -> IO ()
promptSave game = do
  putStrLn "Enter name to save game (press enter to skip)"
  filename <- prompt "> "
  if not $ null filename then do
    saveGame (filename <> ".sudoku") game
    putStrLn $ "Game saved to '" <> (filename <> ".sudoku") <> "'"
  else
    putStrLn "Game not saved"
