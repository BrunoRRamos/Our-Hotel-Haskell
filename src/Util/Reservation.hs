{-# OPTIONS_GHC -Wno-missing-fields #-}

module Util.Reservation (cancelReservation, makeReservation, editReservation, getReservationById) where

import Data.List (find)
import Data.Time
import Database.SQLite.Simple
import Models.Reservation
import Models.Room
import Models.User
import Text.Read (readMaybe)
import Util.IO (askForInput, clearScreen, parseBoolInput, parseDate)

makeReservation :: Connection -> User -> IO (Maybe Reservation)
makeReservation conn user = do
  clearScreen
  reservation <- reservationForm conn user

  reservationId <-
    createReservation
      conn
      reservation
  getReservation conn (fromIntegral reservationId :: Int)

editReservation :: Connection -> User -> IO (Maybe Reservation)
editReservation conn user = do
  clearScreen
  reservation <- getReservationById conn

  if _userId reservation /= Models.User._email user
    then putStrLn "You are not allowed to edit this reservation!" >> return Nothing
    else do
      putStrLn "Reservation found!\n"
      let reservationId = Models.Reservation._id reservation
      form <- reservationForm conn user

      updateReservation conn reservationId (form {Models.Reservation._id = 0})
      getReservation conn reservationId

cancelReservation :: Connection -> User -> IO Bool
cancelReservation conn user = do
  clearScreen
  reservation <- getReservationById conn
  if _userId reservation /= Models.User._email user
    then putStrLn "You are not allowed to cancel this reservation!" >> return False
    else do
      putStrLn "Reservation found!\n"
      cmd <- askForInput "Do you want to cancel this reservation? (y/n)" parseBoolInput
      if cmd
        then deleteReservation conn (Models.Reservation._id reservation) >> return True
        else return False

reservationForm :: Connection -> User -> IO Reservation
reservationForm conn user = do
  start <- askForInput "Enter start date (YYYY-MM-DD): " parseAndValidateDate
  end <- askForInput "Enter end date (YYYY-MM-DD): " $ \str -> do
    day <- parseAndValidateDate str
    maybe
      (return Nothing)
      ( \d ->
          if d < start
            then putStrLn "End date has to be greater than start date! Please try again" >> return Nothing
            else return $ Just d
      )
      day

  rooms <- getAvailableRooms conn start end
  mapM_ printRoom rooms

  room <-
    askForInput
      "Enter room number: "
      ( \input -> do
          let roomId = readMaybe input :: Maybe Int
          case roomId of
            Just _id -> return $ find (\room -> Models.Room._id room == _id) rooms
            Nothing -> return Nothing
      )
  blockServices <- askForInput "Block services? (y/n): " parseBoolInput

  return
    Reservation
      { _roomId = Models.Room._id room,
        _start = start,
        _end = end,
        _blockServices = blockServices,
        _userId = Models.User._email user,
        _rating = Nothing
      }

validateDate :: Day -> IO (Maybe Day)
validateDate date = do
  currentDay <- utctDay <$> getCurrentTime
  if date >= currentDay then return $ Just date else return Nothing

getReservationById :: Connection -> IO Reservation
getReservationById conn = do
  askForInput
    "\nEnter reservation id: "
    ( \str -> do
        let input = (readMaybe str :: Maybe Int)
        case input of
          Nothing -> putStrLn "Invalid reservation id! Please try again" >> return Nothing
          Just i -> do
            res <- getReservation conn i
            maybe (putStrLn "Reservation not found! Please try again" >> return Nothing) (return . Just) res
    )

parseAndValidateDate :: String -> IO (Maybe Day)
parseAndValidateDate str = maybe (return Nothing) validateDate (parseDate str)

printRoom :: Room -> IO ()
printRoom room =
  putStrLn $ "Room " ++ show (Models.Room._id room) ++ " - $" ++ show (Models.Room._dailyRate room) ++ " per night"