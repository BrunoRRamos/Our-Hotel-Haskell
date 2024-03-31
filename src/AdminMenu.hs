module AdminMenu (adminMenu) where
import Database.SQLite.Simple (Connection)
import Models.User (User)
import AdminMenus.RoomMenu (roomMenu)
import AdminMenus.UsersMenu (usersMenu)

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
      usersMenu conn
      loop args
    "2" -> do
      putStrLn "Rooms"
      roomMenu conn 
      loop args
    "3" -> do
      putStrLn "Services"
      loop args
    "4" -> do
      putStrLn "Hotel Review"
      loop args 
    "5" -> do
      putStrLn "exit"
    _ -> do
      putStrLn "Invalid command"
      loop args
  where
    loop = adminMenu conn user