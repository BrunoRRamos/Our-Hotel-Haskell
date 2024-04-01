{-# LANGUAGE OverloadedStrings #-}

module Util.Database (startDb) where

import Database.SQLite.Simple (Connection, open)
import Models.Reservation (createReservationTable)
import Models.Room (createRoomTable)
import Models.RoomService (createRoomServiceTable)
import Models.Service (createServiceTable)
import Models.User (createUserTable, createUser, User (..), Role (ADMIN), getUser)
import Control.Monad (when)
import Data.Maybe (isNothing)

startDb :: IO Connection
startDb = do
  conn <- open "hotel.db"
  createUserTable conn
  --default admin user
  let user = User {
    _email = "admin@admin.com"
  , _firstName = "Admin"
  , _lastName = "Admin"
  , _password = "admin"
  , _isActive = True
  , _block_reason = Nothing
  , _role = ADMIN 
  }
  userExists <- getUser conn (_email user)
  when (isNothing userExists) $ createUser conn user
  createReservationTable conn
  createRoomTable conn
  createRoomServiceTable conn
  createServiceTable conn
  return conn
