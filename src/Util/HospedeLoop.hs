{-# LANGUAGE OverloadedStrings #-}

module Util.HospedeLoop
    ( hospedeLoop
    ) where

import Util.Hospede (requestRoomService)
import Database (startDb)
import Models.Service (ServiceType (CLEANING, MEAL))
import Models.Reservation (createReservation, Reservation(..), ServiceStatus (..), getReservation)
import System.Exit (die)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Control.Exception (try, SomeException)

instance Eq Reservation where
  (==) res1 res2 = _id res1 == _id res2

hospedeLoop :: [String] -> IO ()
hospedeLoop args = do
  conn <- startDb
  let start = parseTimeOrError True defaultTimeLocale "%F" "2021-12-01"
  let end = parseTimeOrError True defaultTimeLocale "%F" "2021-12-05"
  let res = Reservation
        { _id = 1,
          _roomId = 505,
          _userId = "007@gmail.com",
          _start = start,
          _end = end,
          _blockServices = False,
          _rating = Nothing,
          _serviceStatus = "FREE"
        }
  let resND = Reservation
        { _id = 2,
          _roomId = 300,
          _userId = "teste@gmail.com",
          _start = start,
          _end = end,
          _blockServices = False,
          _rating = Nothing,
          _serviceStatus = "NOT DISTURB"
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
      if reservationId == _id res || reservationId == _id resND
        then do
          maybeReservation <- getReservation conn reservationId
          case maybeReservation of
            Just reservation ->
              if _serviceStatus reservation == "FREE"
                then do
                  putStrLn "Enter price:"
                  price <- readLn :: IO Double
                  putStrLn "Enter description:"
                  description <- getLine
                  result <- try $ requestRoomService conn reservationId price CLEANING description :: IO (Either SomeException ())
                  case result of
                    Left _ -> putStrLn "Failed to request room cleaning."
                    Right _ -> putStrLn "Cleaning service requested successfully!"
                  hospedeLoop args
                else putStrLn "Service status must be FREE to create a service."
            Nothing -> putStrLn "Reservation not found."
        else putStrLn "Reservation ID does not exist."

    "2" -> do
      putStrLn "\nRequesting meal service..."
      putStrLn "Enter reservation ID:"
      reservationId <- readLn :: IO Int
      if reservationId == _id res || reservationId == _id resND
        then do
          maybeReservation <- getReservation conn reservationId
          case maybeReservation of
            Just reservation ->
              if _serviceStatus reservation == "FREE"
                then do
                  putStrLn "Enter price:"
                  price <- readLn :: IO Double
                  putStrLn "Enter description:"
                  description <- getLine
                  result <- try $ requestRoomService conn reservationId price MEAL description :: IO (Either SomeException ())
                  case result of
                    Left _ -> putStrLn "Failed to request meal service."
                    Right _ -> putStrLn "Meal service requested successfully!"
                  hospedeLoop args
                else putStrLn "Service status must be FREE to create a service."
            Nothing -> putStrLn "Reservation not found."
        else putStrLn "Reservation ID does not exist."
    "3"-> putStrLn "Goodbye!"

    _ -> do
      putStrLn "Invalid command. Please try again."
      hospedeLoop args
