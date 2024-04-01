{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.RoomService where

import Database.SQLite.Simple
import GHC.Generics

data RoomService = RoomService
  { _id :: Int,
    _reservationId :: Int,
    _serviceId :: Int
  }
  deriving (Show, Generic)

instance FromRow RoomService

createRoomServiceTable :: Connection -> IO ()
createRoomServiceTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS room_service (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \reservation_id INTEGER NOT NULL,\
    \service_id INTEGER NOT NULL,\
    \FOREIGN KEY (reservation_id) REFERENCES room(id),\
    \FOREIGN KEY (service_id) REFERENCES service(id))"

getAllRoomServices :: Connection -> IO [RoomService]
getAllRoomServices conn = query_ conn "SELECT * FROM room_service" :: IO [RoomService]

getRoomServicesByReservation :: Connection -> Int -> IO [RoomService]
getRoomServicesByReservation conn reservationId = do
  room_services <- getAllRoomServices conn
  return $ filter (\rs -> _reservationId rs == reservationId) room_services

createRoomService :: Connection -> RoomService -> IO ()
createRoomService conn roomService = do
  execute
    conn
    "INSERT INTO room_service (reservation_id, service_id) VALUES (?, ?)"
    (_reservationId roomService, _serviceId roomService)