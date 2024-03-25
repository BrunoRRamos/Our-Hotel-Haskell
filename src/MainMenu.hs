{-# OPTIONS_GHC -Wno-missing-fields #-}

module MainMenu
  ( loop,
  )
where

import Data.Time.Format
import Database (startDb)
import Models.Reservation (Reservation (..), createReservation, getReservation)
import Models.Room (Room (..), createRoom, getRoom)
import Models.Service (Service (..), ServiceType (..), createService, getService, printService, getAllServices)
import Models.User (Role (..), User (..), createUser, getUser)
import Rooms (roomsLoop)
import Utils.Hospede (requestRoomService)
import System.Exit (die)

loop :: [String] -> IO ()
loop args = do
  conn <- startDb
  putStrLn "\nAvailable commands:"
  putStrLn "1.  rooms"
  putStrLn "2.  test - create client"
  putStrLn "3.  test - create room and reservation"
  putStrLn "4.  test - create service"
  putStrLn "5.  test - get all services"
  putStrLn "6.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      roomsLoop args
    -- FOR TESTING PURPOSES
    "2" -> do
      createUser
        conn
        User
          { _firstName = "James",
            _lastName = "Bond",
            _email = "007@gmail.com",
            _password = "password",
            _isActive = True,
            _role = ADMIN
          }
      user <- getUser conn "007@gmail.com"
      print user
      print $ _role user
      loop args
    "3" -> do
      createRoom
        conn
        Room
          { Models.Room._id = 505,
            _dailyRate = 200,
            _status = "AVAILABLE",
            _occupancy = 2
          }
      room <- getRoom conn 505
      print "room created!"
      print room

      let start = parseTimeOrError True defaultTimeLocale "%F" "2021-12-01"
      let end = parseTimeOrError True defaultTimeLocale "%F" "2021-12-05"
      createReservation
        conn
        Reservation
          { _roomId = 505,
            _userId = "007@gmail.com",
            _start = start,
            _end = end,
            _blockServices = False,
            _rating = Nothing
          }
      reservation <- getReservation conn 1
      print reservation

      loop args
    "4" -> do
      requestRoomService conn 3 100 CLEANING "testee"
      -- createService
      --   conn
      --   Service
      --     { _reservationId = 2,
      --       _type = CLEANING,
      --       _price = 100,
      --       _description = "CLEANING"
      --     }
      -- service <- getService conn 1
      -- print service
      loop args
    "5" -> do
      conn <- startDb
      putStrLn "Listing all services:"
      allServices <- getAllServices conn
      mapM_ printService allServices
      loop args
    "6" -> die "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      loop args
  loop args
