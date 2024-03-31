{-# LANGUAGE OverloadedStrings #-}

module Util.Database (startDb) where

import Database.SQLite.Simple ( Connection, open )
import Models.Reservation (createReservationTable)
import Models.Room (createRoomTable)
import Models.Service (createServiceTable)
import Models.User (createUserTable, createUser, User (..), Role (ADMIN))

startDb :: IO Connection
startDb = do
  conn <- open "hotel.db"
  createUserTable conn
  --default admin user
  createUser conn User {
    _email = "admin@admin.com"
  , _firstName = "Admin"
  , _lastName = "Admin"
  , _password = "admin"
  , _isActive = True
  , _block_reason = Nothing
  , _role = ADMIN 
  }
  createReservationTable conn
  createRoomTable conn
  createServiceTable conn
  return conn
