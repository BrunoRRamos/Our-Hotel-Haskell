{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Models.Reservation (module Models.Reservation) where

import Data.List (find)
import Data.Time.Calendar
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.SQLite.Simple
import GHC.Generics

data Reservation = Reservation
  { _id :: Int,
    _roomId :: Int,
    _userId :: String,
    _start :: Day,
    _end :: Day,
    _rating :: Maybe Int,
    _blockServices :: Bool
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
    \FOREIGN KEY (room_id) REFERENCES room(id),\
    \FOREIGN KEY (user_id) REFERENCES user(email))"

createReservation :: Connection -> Reservation -> IO ()
createReservation conn reservation =
  execute
    conn
    "INSERT INTO reservation (room_id, user_id, start, end, rating, block_services) VALUES (?, ?, ?, ?, ?, ?)"
    ( _roomId reservation,
      _userId reservation,
      iso8601Show $ _start reservation,
      iso8601Show $ _start reservation,
      _rating reservation,
      _blockServices reservation
    )

getReservation :: Connection -> Int -> IO Reservation
getReservation conn reservationId = do
  reservations <- query_ conn "SELECT * FROM reservation" :: IO [Reservation]
  let _reservation = find (\reservation -> _id reservation == reservationId) reservations
  case _reservation of
    Just reservation -> return reservation
    Nothing -> error "Reservation not found"

getAllReservations :: Connection -> IO [Reservation]
getAllReservations conn = query_ conn "SELECT * FROM reservation" :: IO [Reservation]