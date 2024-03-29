{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Models.Reservation (module Models.Reservation) where

import Data.List (find)
import Data.Time.Calendar
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics

data ServiceStatus = NOT_DISTURB | FREE deriving (Show, Eq)

instance FromField ServiceStatus where
  fromField :: FieldParser ServiceStatus
  fromField f = do
    _field <- (fromField :: FieldParser String) f
    case _field of
      "NOT DISTURB" -> return NOT_DISTURB
      "FREE" -> return FREE
      _ -> returnError ConversionFailed f "Unknown reservation status"

data Reservation = Reservation
  { _id :: Int,
    _roomId :: Int,
    _userId :: String,
    _start :: Day,
    _end :: Day,
    _rating :: Maybe Int,
    _blockServices :: Bool,
    _serviceStatus :: String
  }
  deriving (Show, Generic)

instance FromRow Reservation

createReservationTable :: Connection -> IO ()
createReservationTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS reservation (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \room_id INTEGER NOT NULL,\
    \user_id TEXT NOT NULL,\
    \start TEXT NOT NULL,\
    \end TEXT NOT NULL,\
    \rating INTEGER,\
    \block_services BOOLEAN NOT NULL DEFAULT 0,\
    \serviceStatus TEXT CHECK(serviceStatus IN ('NOT DISTURB', 'FREE')) NOT NULL,\
    \FOREIGN KEY (room_id) REFERENCES room(id),\
    \FOREIGN KEY (user_id) REFERENCES user(email))"

createReservation :: Connection -> Reservation -> IO ()
createReservation conn reservation =
  execute
    conn
    "INSERT INTO reservation (room_id, user_id, start, end, rating, block_services, serviceStatus) VALUES (?, ?, ?, ?, ?, ?, ?)"
    ( _roomId reservation,
      _userId reservation,
      iso8601Show $ _start reservation,
      iso8601Show $ _start reservation,
      _rating reservation,
      _blockServices reservation,
      _serviceStatus reservation
    )

getReservation :: Connection -> Int -> IO (Maybe Reservation)
getReservation conn reservationId = do
  reservations <- query_ conn "SELECT * FROM reservation" :: IO [Reservation]
  let maybeReservation = find (\reservation -> _id reservation == reservationId) reservations
  return maybeReservation


getAllReservations :: Connection -> IO [Reservation]
getAllReservations conn = query_ conn "SELECT * FROM reservation" :: IO [Reservation]