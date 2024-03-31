module AdminMenu (adminMenu) where
import Database.SQLite.Simple (Connection)
import Models.User (User, blockClient)
import AdminMenus.RoomMenu (roomMenu)

adminMenu :: Connection -> User -> [String] -> IO ()
adminMenu conn user args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  Users"
  putStrLn "2.  Rooms"
  putStrLn "3.  Services"
  putStrLn "4.  Reservations"
  putStrLn "5.  Block Client"
  putStrLn "6.  exit - Quit the program"
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
      loop args
    "4" -> do
      putStrLn "Reservations"
      loop args
    "5" -> do
      putStrLn "Enter the ID of the client you want to block: "
      clientId <- getLine
      putStrLn "Enter the reason for blocking the client: "
      reason <- getLine
      blockClient conn clientId reason
      loop args
    "6" -> do
      putStrLn "exit"
    _ -> do
      putStrLn "Invalid command"
      loop args
  where
    loop = adminMenu conn user