module Rooms
    ( roomsLoop
    ) where

roomsLoop :: [String] -> IO ()
roomsLoop args = do
  putStrLn "\nAvailable commands:"
  putStrLn "  list_rooms"
  putStrLn "  get_room"
  putStrLn "  service"
  putStrLn "  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "list_hotels" -> do
      roomsLoop args
    "get_hotel" -> do
      roomsLoop args
    "exit" -> putStrLn "Goodbye!"
    _ -> do
        putStrLn "Invalid command. Please try again."
        roomsLoop args
