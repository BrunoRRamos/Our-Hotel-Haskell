{-# LANGUAGE OverloadedStrings #-}

module Util.Database (startDb) where

import Database.SQLite.Simple ( Connection, open )
import Models.Reservation (createReservationTable)
import Models.Room (createRoomTable)
import Models.Service (createServiceTable)
import Models.User (createUserTable)

startDb :: IO Connection
startDb = do
  conn <- open "hotel.db"
  createUserTable conn
  createReservationTable conn
  createRoomTable conn
  createServiceTable conn
  return conn
