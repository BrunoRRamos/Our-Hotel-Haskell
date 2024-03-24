{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Room (module Models.Room) where

import Data.List (find)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics

data RoomStatus = AVAILABLE | OCCUPIED | BLOCKED deriving (Show, Eq)

instance FromField RoomStatus where
  fromField :: FieldParser RoomStatus
  fromField f = do
    _field <- (fromField :: FieldParser String) f
    case _field of
      "AVAILABLE" -> return AVAILABLE
      "OCCUPIED" -> return OCCUPIED
      "BLOCKED" -> return BLOCKED
      _ -> returnError ConversionFailed f "Unknown room status"

data Room = Room
  { _id :: Int,
    _dailyRate :: Double,
    _status :: String,
    _occupancy :: Int
  }
  deriving (Show, Generic)

instance FromRow Room

createRoomTable :: Connection -> IO ()
createRoomTable conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS room (\
    \id INTEGER PRIMARY KEY,\
    \daily_rate REAL NOT NULL,\
    \status TEXT CHECK(status IN ('AVAILABLE', 'OCCUPIED', 'BLOCKED')) NOT NULL,\
    \occupancy INTEGER NOT NULL)"

createRoom :: Connection -> Room -> IO ()
createRoom conn room = do
  execute
    conn
    "INSERT INTO room (id, daily_rate, status, occupancy) VALUES (?, ?, ?, ?)"
    (_id room, _dailyRate room, _status room, _occupancy room)

getRoom :: Connection -> Int -> IO Room
getRoom conn roomId = do
  rooms <- query_ conn "SELECT * FROM room" :: IO [Room]
  let _room = find (\room -> _id room == roomId) rooms
  case _room of
    Just room -> return room
    Nothing -> error "Room not found"