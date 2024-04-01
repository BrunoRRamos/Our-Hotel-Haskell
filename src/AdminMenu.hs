module AdminMenu (adminMenu) where

import AdminMenus.ReviewHotelMenu (hotelReviewMenu)
import AdminMenus.RoomMenu (roomMenu)
import AdminMenus.ServiceMenu (serviceMenu)
import Database.SQLite.Simple (Connection)
import ClientMenus.ChatMenu (chatMenu)
import Models.User (User)
import AdminMenus.UsersMenu (usersMenu)
import System.Exit (die)

adminMenu :: Connection -> User -> [String] -> IO ()
adminMenu conn user args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  Users"
  putStrLn "2.  Rooms"
  putStrLn "3.  Services"
  putStrLn "4.  Hotel Review"
  putStrLn "5.  Chat"
  putStrLn "6.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      putStrLn "\n--- Users ---"
      usersMenu conn
      loop args
    "2" -> do
      putStrLn "\n--- Rooms ---"
      roomMenu conn
      loop args
    "3" -> do
      putStrLn "\n--- Services ---"
      serviceMenu conn
      loop args
    "4" -> do
      putStrLn "\n--- Hotel Review ---"
      hotelReviewMenu conn
      loop args
    "5" -> do
      putStrLn "\n--- Chat ----"
      chatMenu conn args
      loop args 
    "6" -> do
      putStrLn "╔══════════════════════════════════════════════════════════════════════════════╗"
      putStrLn "║                    THANK YOU FOR VISITING, COME BACK SOON!                   ║"
      putStrLn "║══════════════════════════════════════════════════════════════════════════════║"
      putStrLn "║                                    TEAM:                                     ║"
      putStrLn "║══════════════════════════════════════════════════════════════════════════════║"
      putStrLn "║                               Bruno Rodrigues                                ║"
      putStrLn "║                              José Gabriel Melo                               ║"
      putStrLn "║                             Pedro Henrique Costa                             ║"
      putStrLn "║                              Pedro Silva Filho                               ║"
      putStrLn "║                                Suelen Felix                                  ║"
      putStrLn "╚══════════════════════════════════════════════════════════════════════════════╝" 
      die "Goodbye!"
    _ -> do
      putStrLn "Invalid command"
      loop args
  where
    loop = adminMenu conn user