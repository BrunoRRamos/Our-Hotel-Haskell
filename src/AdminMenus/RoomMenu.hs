module AdminMenus.RoomMenu (roomMenu) where
import Util.RoomAdmin (createRoomAdmin, listAllRoomsAdmin, editRoom)
import Database.SQLite.Simple (Connection)

roomMenu :: Connection ->  IO ()
roomMenu conn = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  Add room to hotel"
  putStrLn "2.  Edit room"
  putStrLn "3.  List rooms"
  putStrLn "4.  exit - back to admin menu"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      room <- createRoomAdmin conn 
      print room
      loop 
    "2" -> do
      rooms <- listAllRoomsAdmin conn
      print rooms
      _ <- editRoom conn
      loop
    "3" -> do
      rooms <- listAllRoomsAdmin conn
      print rooms
      loop
    "4" -> do
      putStrLn "exit"
    _ -> do
      putStrLn "Invalid command"
      loop
  where
    loop = roomMenu conn