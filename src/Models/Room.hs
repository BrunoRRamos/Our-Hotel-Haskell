{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Models.Room (module Models.Room) where

import Data.Int (Int64)
import Data.List (find)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics

data RoomStatus = AVAILABLE | RESERVED | BLOCKED deriving (Show, Eq)

instance FromField RoomStatus where
  fromField :: FieldParser RoomStatus
  fromField f = do
    _field <- (fromField :: FieldParser String) f
    case _field of
      "AVAILABLE" -> return AVAILABLE
      "RESERVED" -> return RESERVED
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
    \status TEXT CHECK(status IN ('AVAILABLE', 'RESERVED', 'BLOCKED')) NOT NULL,\
    \occupancy INTEGER NOT NULL)"

createRoom :: Connection -> Room -> IO Int64
createRoom conn room = do
  execute
    conn
    "INSERT INTO room (id, daily_rate, status, occupancy) VALUES (?, ?, ?, ?)"
    (_id room, _dailyRate room, _status room, _occupancy room)
  lastInsertRowId conn

getAllRooms :: Connection -> IO [Room]
getAllRooms conn = query_ conn "SELECT * FROM room" :: IO [Room]

getRoom :: Connection -> Int -> IO (Maybe Room)
getRoom conn roomId = do
  rooms <- getAllRooms conn
  return $ find (\room -> _id room == roomId) rooms

toggleRoomReserved :: Connection -> Int -> IO ()
toggleRoomReserved conn roomId = do
  testRoom <- getRoom conn roomId
  execute conn "UPDATE room SET status = 'RESERVED' WHERE id = ?" (Only roomId)

toggleRoomAvailiable :: Connection -> Int -> IO ()
toggleRoomAvailiable conn roomId = do
  testRoom <- getRoom conn roomId
  execute conn "UPDATE room SET status = 'AVAILABLE' WHERE id = ?" (Only roomId)

updateRoom :: Connection -> Room -> IO ()
updateRoom conn room = do
  execute
    conn
    "UPDATE room SET daily_rate = ?, status = ?, occupancy = ? WHERE id = ?"
    (_dailyRate room, _status room, _occupancy room, _id room)