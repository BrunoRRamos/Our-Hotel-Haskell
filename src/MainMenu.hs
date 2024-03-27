{-# OPTIONS_GHC -Wno-missing-fields #-}

module MainMenu
  ( loop,
  )
where

import Database (startDb)
import Models.Service (Service (..), ServiceType (..), createService, getService)
import Models.User (getAllUsers, getUser)
import Rooms (roomsLoop)
import System.Exit (die)
import Util.IO (clearScreen)
import Util.LoginLoop (loginLoop)
import Util.ReservationLoop (reservationLoop)

loop :: [String] -> IO ()
loop args = do
  clearScreen
  conn <- startDb
  loggedUser <- loginLoop conn
  let user = case loggedUser of
        Just u -> u
        Nothing -> error "User not found"
  print user

  putStrLn "\nAvailable commands:"
  putStrLn "1.  Rooms"
  putStrLn "2.  Reservations"
  putStrLn "3.  test - create client"
  putStrLn "4.  test - create service"
  putStrLn "5.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      roomsLoop args
    -- FOR TESTING PURPOSES
    "2" -> do
      _ <- reservationLoop conn user
      loop args
    "3" -> do
      _user <- getUser conn "baseADM@gmail.com"
      allUsers <- getAllUsers conn
      print allUsers
      case _user of
        Just u -> print u
        Nothing -> print "User not found"
      loop args
    "4" -> do
      createService
        conn
        Service
          { _reservationId = 1,
            _type = MEAL,
            _price = 50,
            _description = "A yummy yummy breakfast"
          }
      service <- getService conn 1
      print service
    "5" -> do
      die "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      loop args
  loop args
