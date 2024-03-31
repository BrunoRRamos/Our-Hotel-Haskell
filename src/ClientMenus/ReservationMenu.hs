{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module ClientMenus.ReservationMenu (reservationMenu) where

import Control.Exception (try)
import Control.Monad (void)
import Database.SQLite.Simple
import Models.Reservation
import Models.User
import Util.Reservation (makeReservation, editReservation, cancelReservation)
import Util.IO (OperationCancelledException, pressEnter, clearScreen)

reservationMenu :: Connection -> User -> IO ()
reservationMenu conn user = do
  clearScreen
  putStrLn
    "1. Make a reservation\n\
    \2. Edit a reservation\n\
    \3. Cancel a reservation\n\
    \4. Go back"
  cmd <- getLine

  case cmd of
    "1" -> do
      result <- try (makeReservation conn user) :: IO (Either OperationCancelledException (Maybe Reservation))
      case result of
        Left _ -> void (putStrLn "Operation cancelled!")
        Right maybeReservation -> case maybeReservation of
          Just _ -> putStrLn "Reservation made successfully!"
          Nothing -> putStrLn "Failed to make a reservation."

      pressEnter
      reservationMenu conn user
    "2" -> do
      result <- try (editReservation conn user) :: IO (Either OperationCancelledException (Maybe Reservation))
      case result of
        Left _ -> void (putStrLn "Operation cancelled!")
        Right maybeReservation -> case maybeReservation of
          Just _ -> putStrLn "Reservation updated successfully!"
          Nothing -> putStrLn "Failed to update the reservation."
      pressEnter
      reservationMenu conn user
    "3" -> do
      result <- try (cancelReservation conn user) :: IO (Either OperationCancelledException Bool)
      case result of
        Left _ -> void (putStrLn "Operation cancelled!")
        Right res -> if res then putStrLn "Reservation canceled successfully!" else putStrLn "Reservation not canceled!"
      pressEnter
      reservationMenu conn user
    "4" -> return ()
    _ -> do
      print "Invalid command. Please try again"
      reservationMenu conn user
