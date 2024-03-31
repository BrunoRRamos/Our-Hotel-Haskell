module Util.IO (module Util.IO) where

import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Data.Data (Typeable)
import Data.Time

data OperationCancelledException = OperationCancelledException deriving (Show, Typeable)

instance Exception OperationCancelledException

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseBoolInput :: String -> IO (Maybe Bool)
parseBoolInput str = case str of
  "y" -> return $ Just True
  "n" -> return $ Just False
  _ -> putStrLn "Invalid input. Please try again." >> return Nothing

clearScreen :: IO ()
clearScreen = do
  putStr "\ESC[2J"

askForInput :: String -> (String -> IO (Maybe t)) -> IO t
askForInput prompt validate = do
  putStrLn $ prompt ++ " (enter c to cancel)"
  _input <- getLine
  case _input of
    "c" -> throw OperationCancelledException
    _ -> return ()
  value <- validate _input
  case value of
    Just v -> do
      return v
    Nothing -> do
      askForInput prompt validate

pressEnter :: IO ()
pressEnter = do
  putStrLn "\nPress enter to go back"
  _ <- getLine
  return ()

askForInputWithDefault :: String -> (String -> IO (Maybe t)) -> t -> IO t
askForInputWithDefault prompt validate defaultValue = do
  putStrLn $ prompt ++ " (optional) (enter c to cancel)"
  _input <- getLine
  case _input of
    "c" -> error "Operation cancelled"
    _ -> return ()
  if _input == ""
    then return defaultValue
    else do
        value <- validate _input
        case value of
          Just v -> do
            return v
          Nothing -> do
            askForInputWithDefault prompt validate defaultValue
  
