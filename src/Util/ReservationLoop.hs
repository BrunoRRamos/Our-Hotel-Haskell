{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Util.ReservationLoop (reservationLoop) where

import Control.Exception.Base (SomeException, catch)
import Data.List (find)
import Data.Time
import Database.SQLite.Simple
import Models.Reservation
import Models.Room
import Models.User
import Text.Read (readMaybe)
import Util.IO (askForInput, clearScreen, parseDate)

reservationLoop :: Connection -> User -> IO (Maybe Reservation)
reservationLoop conn user = do
  clearScreen
  putStrLn
    "1. Make a reservation\n\
    \2. Cancel a reservation\n\
    \3. Go back"
  cmd <- getLine

  case cmd of
    "1" -> do
      reservation <-
        catch
          (makeReservation conn user)
          ( \e ->
              do
                let _ = (e :: SomeException)
                return Nothing
          )
      case reservation of
        Nothing -> putStrLn "An error occurred while"
        Just r -> do
          putStrLn "Room booked successfully!"
          putStrLn $ "Reservation id is:" ++ show (Models.Reservation._id r)

      putStrLn "\nPress enter to go back"
      _ <- getLine

      reservationLoop conn user
    -- "2" -> cancelReservation conn
    "3" -> return Nothing
    _ -> do
      print "Invalid command. Please try again"
      reservationLoop conn user

makeReservation :: Connection -> User -> IO (Maybe Reservation)
makeReservation conn user = do
  clearScreen
  start <- askForInput "Enter start date (YYYY-MM-DD): " parseAndValidateDate
  end <- askForInput "Enter end date (YYYY-MM-DD): " $ \str -> do
    day <- parseAndValidateDate str
    case day of
      Just d ->
        if d < start
          then do
            putStrLn "End date has to greater than start date! Please try again"
            return Nothing
          else return $ Just d
      Nothing -> return Nothing

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
  blockServices <- askForInput "Block services? (y/n): " parseBlockServices

  reservationId <-
    createReservation
      conn
      Reservation
        { _roomId = Models.Room._id room,
          _start = start,
          _end = end,
          _blockServices = blockServices,
          _userId = Models.User._email user,
          _rating = Nothing
        }
  print reservationId
  getReservation conn (fromIntegral reservationId :: Int)
  where
    validateDate :: Day -> IO (Maybe Day)
    validateDate date = do
      currentDay <- utctDay <$> getCurrentTime
      if date >= currentDay then return $ Just date else return Nothing

    parseAndValidateDate :: String -> IO (Maybe Day)
    parseAndValidateDate str = case parseDate str of
      Just date -> validateDate date
      Nothing -> clearScreen >> putStrLn "Invalid date. Please try again." >> return Nothing

    parseBlockServices :: String -> IO (Maybe Bool)
    parseBlockServices str = case str of
      "y" -> return $ Just True
      "n" -> return $ Just False
      _ -> putStrLn "Invalid input. Please try again." >> return Nothing

    printRoom :: Room -> IO ()
    printRoom room =
      putStrLn $ "Room " ++ show (Models.Room._id room) ++ " - $" ++ show (Models.Room._dailyRate room) ++ " per night"

-- let start = fromGregorian 2024 12 1
-- let end = fromGregorian 2024 12 15
-- print $ overlap start end Reservation {_start = fromGregorian 2024 12 1, _end = fromGregorian 2024 12 15} -- same day
-- print $ overlap start end Reservation {_start = fromGregorian 2024 11 20, _end = fromGregorian 2024 12 12} -- start before
-- print $ overlap start end Reservation {_start = fromGregorian 2024 12 12, _end = fromGregorian 2024 12 30} -- end after
-- print $ overlap start end Reservation {_start = fromGregorian 2024 12 12, _end = fromGregorian 2024 12 14} -- start and end inside
-- print $ overlap start end Reservation {_start = fromGregorian 2024 11 30, _end = fromGregorian 2024 12 20} -- start before and end after
-- print $ overlap start end Reservation {_start = fromGregorian 2024 11 10, _end = fromGregorian 2024 11 12} -- should be false
-- print $ overlap start end Reservation {_start = fromGregorian 2024 12 1, _end = fromGregorian 2024 12 12}
-- print $ overlap start end Reservation {_start = fromGregorian 2024 12 10, _end = fromGregorian 2024 12 15}
-- print $ overlap start end Reservation {_start = fromGregorian 2024 11 10, _end = fromGregorian 2024 12 1}
-- print $ overlap start end Reservation {_start = fromGregorian 2024 12 15, _end = fromGregorian 2024 12 18}
-- priquita <- getLine