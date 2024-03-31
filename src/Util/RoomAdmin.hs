{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Util.RoomAdmin (createRoomAdmin, listAllRoomsAdmin, editRoom) where

import Database.SQLite.Simple (Connection)
import Models.Room (Room(..), createRoom, getRoom, getAllRooms, updateRoom)
import Util.IO (clearScreen, askForInput, askForInputWithDefault)
import Text.Read (readMaybe)


createRoomAdmin :: Connection -> IO (Maybe Room)
createRoomAdmin conn = do
  clearScreen
  roomNumber <- askForInput "Enter room number: " (parseAndValidateRoomNumber conn)
  dailyRate <- askForInput "Enter daily rate: " parseDouble
  occupancy <- askForInput "Enter occupancy: " parseInt

  roomId <- createRoom conn Room {
    _id = roomNumber,
    _dailyRate = dailyRate,
    _status = "AVAILABLE",
    _occupancy = occupancy
  }

  getRoom conn (fromIntegral roomId)

listAllRoomsAdmin :: Connection -> IO [Room]
listAllRoomsAdmin conn = do
  getAllRooms conn

editRoom :: Connection -> IO (Maybe Room)
editRoom conn = do
  roomId <- askForInput "Enter room number: " parseAndValidateEditRoomNumber 
  room <- getRoom conn roomId
  case room of
    Nothing -> putStrLn "Room not found" >> return Nothing
    Just r -> do
      putStrLn "Edit room"
      putStrLn $ "Room number: " ++ show (_id r)
      putStrLn $ "Daily rate: " ++ show (_dailyRate r)
      putStrLn $ "Occupancy: " ++ show (_occupancy r)
      putStrLn "Enter new values"
      dailyRate <- askForInputWithDefault "Enter daily rate: " parseDouble (_dailyRate r)
      occupancy <- askForInputWithDefault "Enter occupancy: " parseInt (_occupancy r)
      status <- askForInputWithDefault "Enter status: BLOCKED(b) OR AVAILABLE(a) " parseAndValidateStatus (_status r)
      
      updateRoom conn Room {
        _id = _id r,
        _dailyRate = dailyRate,
        _status = status,
        _occupancy = occupancy
      }

      putStrLn "Room updated successfully"
      getRoom conn (_id r)
  where 
    parseAndValidateStatus :: String -> IO (Maybe String)
    parseAndValidateStatus str = do
      case str of
        "a" -> return (Just "AVAILABLE")
        "b" -> return (Just "BLOCKED")
        _ -> return Nothing 

    parseAndValidateEditRoomNumber :: String -> IO (Maybe Int)
    parseAndValidateEditRoomNumber str = do
      let roomNumber = readMaybe str :: Maybe Int
      rooms <- getAllRooms conn
      case roomNumber of
        Nothing -> return Nothing
        Just n -> do
          if any (\room -> _id room == n) rooms
            then return (Just n)
            else return Nothing

parseAndValidateRoomNumber :: Connection -> String -> IO (Maybe Int)
parseAndValidateRoomNumber conn str = do
  let roomNumber = readMaybe str :: Maybe Int
  rooms <- getAllRooms conn
  case roomNumber of
    Nothing -> return Nothing
    Just n -> do
      if any (\room -> _id room == n) rooms
        then return Nothing
        else return (Just n)


parseInt :: String -> IO (Maybe Int)
parseInt str = return (readMaybe str :: Maybe Int)

parseDouble :: String -> IO (Maybe Double)
parseDouble str = return (readMaybe str :: Maybe Double)