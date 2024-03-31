{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ClientMenus.RoomServiceMenu
    (roomServiceMenu 
    ) where

import Util.RoomService (requestRoomService)
import Models.Service (ServiceType (CLEANING, MEAL))
import Models.Reservation (Reservation(..))
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Control.Exception (try, SomeException)
import Database.SQLite.Simple (Connection)

instance Eq Reservation where
  (==) res1 res2 = _id res1 == _id res2

roomServiceMenu :: Connection -> [String] -> IO ()
roomServiceMenu conn args = do
  let start = parseTimeOrError True defaultTimeLocale "%F" "2021-12-01"
  let end = parseTimeOrError True defaultTimeLocale "%F" "2021-12-05"
  let res = Reservation
        { _id = 1,
          _roomId = 505,
          _userId = "007@gmail.com",
          _start = start,
          _end = end,
          _blockServices = False,
          _rating = Nothing
        }
  putStrLn "\nAvailable commands:"
  putStrLn "1. Request room cleaning"
  putStrLn "2. Request meal service"
  putStrLn "3. Exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      putStrLn "\nRequesting room cleaning..."
      putStrLn "Enter reservation ID:"
      reservationId <- readLn :: IO Int
      if reservationId == _id res
        then do
          putStrLn "Enter price:"
          price <- readLn :: IO Double
          putStrLn "Enter description:"
          description <- getLine
          result <- try $ requestRoomService conn reservationId price CLEANING description :: IO (Either SomeException ())
          case result of
            Left _ -> putStrLn "Failed to request room cleaning."
            Right _ -> putStrLn "Cleaning service requested successfully!"
          roomServiceMenu conn args
        else putStrLn "Reservation ID does not exist."

    "2" -> do
      putStrLn "\nRequesting meal service..."
      putStrLn "Enter reservation ID:"
      reservationId <- readLn :: IO Int
      if reservationId == _id res
        then do
          putStrLn "Enter price:"
          price <- readLn :: IO Double
          putStrLn "Enter description:"
          description <- getLine
          result <- try $ requestRoomService conn reservationId price MEAL description :: IO (Either SomeException ())
          case result of
            Left _ -> putStrLn "Failed to request meal service."
            Right _ -> putStrLn "Meal service requested successfully!"
          roomServiceMenu conn args
        else putStrLn "Reservation ID does not exist."

    "3"-> putStrLn "Goodbye!"

    _ -> do
      putStrLn "Invalid command. Please try again."
      roomServiceMenu conn args
