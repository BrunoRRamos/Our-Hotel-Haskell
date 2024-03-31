{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ClientMenus.RoomServiceMenu (roomServiceMenu) where

import Control.Exception (try)
import Control.Monad (void)
import Database.SQLite.Simple (Connection)
import Models.Service (ServiceType (CLEANING, MEAL))
import Models.User (User)
import Util.IO (OperationCancelledException, clearScreen, pressEnter)
import Util.RoomService (requestRoomService)

roomServiceMenu :: Connection -> User -> IO ()
roomServiceMenu conn user = do
  clearScreen
  putStrLn "\nAvailable commands:"
  putStrLn "1. Request room cleaning"
  putStrLn "2. Request meal service"
  putStrLn "3. Exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  case cmd of
    "1" -> do
      result <- try (requestRoomService conn user CLEANING) :: IO (Either OperationCancelledException ())
      case result of
        Left _ -> void (putStrLn "Operation cancelled!")
        Right _ -> putStrLn "Service room requested successfully!"
      pressEnter
      roomServiceMenu conn user
    "2" -> do
      result <- try (requestRoomService conn user MEAL) :: IO (Either OperationCancelledException ())
      case result of
        Left _ -> void (putStrLn "Operation cancelled!")
        Right _ -> putStrLn "Service room requested successfully!"
      pressEnter
      roomServiceMenu conn user
    "3" -> putStrLn "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      roomServiceMenu conn user
