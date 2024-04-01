{-# OPTIONS_GHC -Wno-missing-fields #-}

module Util.Reservation (cancelReservation, makeReservation, editReservation, getReservationById, reservationOverview) where

import Control.Exception
import Control.Monad (forM_)
import Data.List (find, group, sortOn)
import Data.Maybe (fromJust)
import Data.Time
import Database.SQLite.Simple
import Models.Reservation
import Models.Room
import Models.RoomService (RoomService (_serviceId), getRoomServicesByReservation)
import Models.Service (Service (..), getAllServices)
import Models.User
import Text.Printf
import Text.Read (readMaybe)
import Util.IO (OperationCancelledException (OperationCancelledException), askForInput, clearScreen, parseBoolInput, parseDate)

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
  if null rooms 
    then do
     noRoomsAvaliable
     return ()
  else mapM_ printRoom rooms

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
      { Models.Reservation._roomId = Models.Room._id room,
        _start = start,
        _end = end,
        _blockServices = blockServices,
        _userId = Models.User._email user,
        _rating = Nothing
      }

noRoomsAvaliable :: IO ()
noRoomsAvaliable = do
  putStr "No rooms availiable for these dates.\n"
  

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

reservationOverview :: Connection -> User -> IO ()
reservationOverview conn user = do
  clearScreen
  reservation <- getReservationById conn
  if _userId reservation /= Models.User._email user
    then putStrLn "You are not allowed to view this reservation!" >> throw OperationCancelledException
    else putStrLn "Reservation found!\n"

  room <- fromJust <$> getRoom conn (Models.Reservation._roomId reservation)
  roomServices <- getRoomServicesByReservation conn (Models.Reservation._id reservation)
  reservationServices <- getServicesByRoomServices roomServices

  let totalDays = diffDays (_end reservation) (_start reservation)
  let servicesTotal = sum $ map Models.Service._price reservationServices
  let accommodationsTotal = Models.Room._dailyRate room * fromIntegral totalDays

  let sortedServices = sortOn Models.Service._id reservationServices
  let groupedServices = group sortedServices

  clearScreen
  putStrLn "########## Items ##########"
  putStrLn
    ( printf
        "%-17.17s"
        ("Room " ++ show (Models.Room._id room))
        ++ " - "
        ++ "$"
        ++ show (Models.Room._dailyRate room)
        ++ " x"
        ++ show totalDays
        ++ " "
        ++ ( if totalDays == 1
               then "night"
               else
                 "nights"
           )
    )
  forM_ groupedServices $ \rs -> do
    let service = head rs
    let desc = printf "%-17.17s" (Models.Service._description service)
    putStrLn $
      desc
        ++ " - $"
        ++ show (Models.Service._price service)
        ++ " x"
        ++ show (length rs)

  putStrLn
    ( "\n######### Details #########\n"
        ++ "Services ---------- $"
        ++ show servicesTotal
        ++ "\nAccommodations ---- $"
        ++ show accommodationsTotal
        ++ "\n\nTotal ------------- $"
        ++ show (servicesTotal + accommodationsTotal)
    )
  where
    getServicesByRoomServices :: [RoomService] -> IO [Service]
    getServicesByRoomServices roomServices = do
      services <- getAllServices conn
      return $ map (\rs -> fromJust $ find (\service -> Models.Service._id service == _serviceId rs) services) roomServices