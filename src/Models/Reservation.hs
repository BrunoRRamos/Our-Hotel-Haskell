{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Models.Reservation (module Models.Reservation) where

import Data.Maybe (fromMaybe)
import Models.Room (toggleRoomAvailiable, toggleRoomReserved, getRoom )
import Data.Int (Int64)
import Data.List (find)
import Data.Time.Calendar
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.SQLite.Simple
import GHC.Generics
import Models.Room (Room, getAllRooms)
import qualified Models.Room as Models

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

createReservation :: Connection -> Reservation -> IO Int64
createReservation conn reservation = do
  execute
    conn
    "INSERT INTO reservation (room_id, user_id, start, end, rating, block_services) VALUES (?, ?, ?, ?, ?, ?)"
    ( _roomId reservation,
      _userId reservation,
      iso8601Show $ _start reservation,
      iso8601Show $ _end reservation,
      _rating reservation,
      _blockServices reservation
    )
  lastInsertRowId conn

getReservation :: Connection -> Int -> IO (Maybe Reservation)
getReservation conn reservationId = do
  reservations <- query_ conn "SELECT * FROM reservation" :: IO [Reservation]
  return $ find (\reservation -> _id reservation == reservationId) reservations

getAllReservations :: Connection -> IO [Reservation]
getAllReservations conn = query_ conn "SELECT * FROM reservation" :: IO [Reservation]

getAvailableRooms :: Connection -> Day -> Day -> IO [Room]
getAvailableRooms conn s e = do
  reservations <- getAllReservations conn
  rooms <- getAllRooms conn
  return $ filter (isAvailable reservations) rooms
  where
    isAvailable :: [Reservation] -> Room -> Bool
    isAvailable reservations room = do
      let noReservations = null $ find (\reservation -> _roomId reservation == Models._id room && overlap s e reservation) reservations
      Models._status room /= "BLOCKED" && (noReservations || all (\reservation -> _roomId reservation == Models._id room && not (overlap s e reservation)) reservations)

overlap :: Day -> Day -> Reservation -> Bool
overlap start end reservation =
  start == _start reservation
    || end == _end reservation
    || start == _end reservation
    || end == _start reservation
    || start > _start reservation && start < _end reservation -- starts before and end before
    || end > _start reservation && end < _end reservation -- starts after and end after
    || start < _start reservation && end > _end reservation -- starts after and ends before
    || start < _start reservation && end < _end reservation -- starts before and end after

deleteReservation :: Connection -> Int -> IO ()
deleteReservation conn reservationId = do
  execute conn "DELETE FROM reservation WHERE id = ?" (Only reservationId)

updateReservation :: Connection -> Int -> Reservation -> IO ()
updateReservation conn reservationId reservation = do
  execute
    conn
    "UPDATE reservation SET room_id = ?, user_id = ?, start = ?, end = ?, rating = ?, block_services = ? WHERE id = ?"
    ( _roomId reservation,
      _userId reservation,
      iso8601Show $ _start reservation,
      iso8601Show $ _end reservation,
      _rating reservation,
      _blockServices reservation,
      reservationId
    )

getRoomId :: Connection -> Int -> IO Int
getRoomId conn reservationId = do
  maybeReservation <- getReservation conn reservationId
  let reservation = fromMaybe (error "Reservation not found") maybeReservation
  return (_roomId reservation)

checkIn :: Connection -> Int -> IO ()
checkIn conn reservationId = do
  roomId <- getRoomId conn reservationId
  toggleRoomReserved conn roomId
  putStr "CheckIn done, Welcome !"

checkOut :: Connection -> Int -> IO ()
checkOut conn reservationId = do
  roomId <- getRoomId conn reservationId
  toggleRoomAvailiable conn roomId
  -- Por função de avaliação
  -- Por função que salva dados da estadia
  putStr "CheckOut done, GoodBye !"