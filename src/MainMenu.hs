module MainMenu
    ( loop
    ) where

import Rooms ( roomsLoop )
import System.Exit ( die )
import CreateUser (createUser)

loop :: [String] -> IO ()
loop args = do
  putStrLn "\nAvailable commands:"
  putStrLn "  clients"
  putStrLn "  room"
  putStrLn "  service"
  putStrLn " test"
  putStrLn "  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "room" -> do
      roomsLoop args
    "service" -> do
      loop args
    "exit" -> do
      die "Goodbye!"
    "test" -> do
      putStrLn "Test"
      createUser
      loop args
    _ -> do
        putStrLn "Invalid command. Please try again."
        loop args
  loop args

