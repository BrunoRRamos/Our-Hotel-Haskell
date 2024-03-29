module MainMenu
  ( loop,
  )
where

import Data.Time.Format
import Database (startDb)
import Models.Reservation (Reservation (..), createReservation, getReservation, getAllReservations)
import Models.Room (Room (..), createRoom, getRoom)
import Models.Service (Service (..), ServiceType (..), createService, getAllServices, getService)
import Models.User (Role (..), User (..), createUser, getAllUsers, getUser)
import Rooms (roomsLoop)
import System.Exit (die)
import Util.HospedeLoop (hospedeLoop)
import Util.LoginLoop (loginLoop)

loop :: [String] -> IO ()
loop args = do
  conn <- startDb
  loginLoop args
  putStrLn "\nAvailable commands:"
  putStrLn "1.  Rooms"
  putStrLn "2.  test - create client"
  putStrLn "3.  test - create room and reservation"
  putStrLn "4.  test - create service"
  putStrLn "5.  test - get all services"
  putStrLn "6.  get all reservations"
  putStrLn "7.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "0" -> do
      loginLoop args
    "1" -> do
      roomsLoop args
    -- FOR TESTING PURPOSES
    "2" -> do
      user <- getUser conn "baseADM@gmail.com"
      allUsers <- getAllUsers conn
      print user
      print allUsers
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
            _rating = Nothing,
            _serviceStatus = "FREE"
          }
      reservation <- getReservation conn 1
      print reservation

      loop args
    "4" -> do
      hospedeLoop args
    "5" -> do
      conn <- startDb
      putStrLn "Listing all services:"
      allServices <- getAllServices conn
      putStrLn $ unlines (map show allServices)
      loop args
    "6" -> do
      conn <- startDb
      putStrLn "Listing all reservations:"
      allReservation <- getAllReservations conn
      putStrLn $ unlines (map show allReservation)
      loop args
    "7" -> die "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      loop args
  loop args
