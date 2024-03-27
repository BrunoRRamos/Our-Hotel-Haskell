module Util.IO (module Util.IO) where

import Data.Time

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

clearScreen :: IO ()
clearScreen = do
  putStr "\ESC[2J"

input :: String -> IO String
input prompt = do
  putStrLn prompt
  getLine

askForInput :: String -> (String -> IO (Maybe t)) -> IO t
askForInput prompt validate = do
  putStrLn $ prompt ++ " (enter c to cancel)"
  _input <- getLine
  case _input of
    "c" -> error "Operation cancelled"
    _ -> return ()
  value <- validate _input
  case value of
    Just v -> do
      return v
    Nothing -> do
      askForInput prompt validate