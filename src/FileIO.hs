module FileIO
  ( loadGame
  , saveGame
  , prompt
  , promptSave
  ) where

import Game

import System.IO (hFlush, stdout)

exportGame :: Game -> String
exportGame = show

importGame :: String -> Game
importGame = read

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
  putStrLn "Enter filename to save game (press enter to skip)"
  filename <- prompt "> "
  if not $ null filename
    then do
      saveGame filename game
      putStrLn $ "Game saved to '" ++ filename ++ "'"
    else putStrLn "Game not saved"
