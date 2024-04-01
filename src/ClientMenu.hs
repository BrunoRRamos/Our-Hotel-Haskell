module ClientMenu (clientMenu) where

import ClientMenus.ReservationMenu (reservationMenu)
import ClientMenus.RoomServiceMenu (roomServiceMenu)
import ClientMenus.CheckInOutMenu (checkInOutMenu)
import ClientMenus.ChatMenu (chatMenu)
import Database.SQLite.Simple (Connection)
import Models.User (User)
import System.Exit (die)

clientMenu :: Connection -> User -> [String] -> IO ()
clientMenu conn user args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  Reservations"
  putStrLn "2.  Service"
  putStrLn "3.  Check-In / Check-Out"
  putStrLn "4.  Chat"
  putStrLn "5.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      _ <- reservationMenu conn user
      loop
    "2" -> do
      roomServiceMenu conn user
      loop
    "3" -> do
      checkInOutMenu conn args
      loop
    "4" -> do
      chatMenu conn args
      loop
    "5" -> die "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      loop
  where
    loop = clientMenu conn user args