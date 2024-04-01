module AdminMenu (adminMenu) where

import AdminMenus.ReviewHotelMenu (hotelReviewMenu)
import AdminMenus.RoomMenu (roomMenu)
import AdminMenus.ServiceMenu (serviceMenu)
import Database.SQLite.Simple (Connection)
import Models.User (User)

adminMenu :: Connection -> User -> [String] -> IO ()
adminMenu conn user args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  Users"
  putStrLn "2.  Rooms"
  putStrLn "3.  Services"
  putStrLn "4.  Hotel Review"
  putStrLn "5.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      putStrLn "Users"
      loop args
    "2" -> do
      putStrLn "Rooms"
      roomMenu conn
      loop args
    "3" -> do
      putStrLn "Services"
      serviceMenu conn
      loop args
    "4" -> do
      putStrLn "Hotel Review"
      hotelReviewMenu conn
      loop args
    "5" -> do
      putStrLn "exit"
    _ -> do
      putStrLn "Invalid command"
      loop args
  where
    loop = adminMenu conn user