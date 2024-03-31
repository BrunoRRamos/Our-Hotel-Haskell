module ClientMenu (clientMenu) where

import Database.SQLite.Simple (Connection)
import Models.User (User, getUser, getAllUsers)
import ClientMenus.RoomsMenu (roomsMenu)
import ClientMenus.ReservationMenu (reservationMenu)
import ClientMenus.RoomServiceMenu (roomServiceMenu)
import Models.Service (getAllServices)
import System.Exit (die)


clientMenu :: Connection -> User -> [String] -> IO ()
clientMenu conn user args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  Rooms"
  putStrLn "2.  Reservations"
  putStrLn "3.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      roomsMenu conn args
    -- FOR TESTING PURPOSES
    "2" -> do
      _ <- reservationMenu conn user
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
      roomServiceMenu conn args
    "5" -> do
      putStrLn "Listing all services:"
      allServices <- getAllServices conn
      putStrLn $ unlines (map show allServices)
      loop args
    "6" -> die "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      loop args
  where
    loop = clientMenu conn user