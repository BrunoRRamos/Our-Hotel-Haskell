module AdminMenus.UsersMenu (usersMenu) where

import Database.SQLite.Simple (Connection)
import Models.User (blockClient)

usersMenu :: Connection -> IO ()
usersMenu conn  = do
  putStrLn "Available Commands"
  putStrLn "1. List Users"
  putStrLn "2. Add Admin User"
  putStrLn "3. Delete User"
  putStrLn "4. Block Client User"
  putStrLn "5. Exit"
  putStrLn "Enter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      putStrLn "List Users"
      loop 
    "2" -> do
      putStrLn "Add Admin User"
      loop
    "3" -> do
      putStrLn "Delete User"
      loop
    "4" -> do
      putStrLn "Update User"
      putStrLn "Enter the ID of the client you want to block: "
      clientId <- getLine
      putStrLn "Enter the reason for blocking the client: "
      reason <- getLine
      blockClient conn clientId reason
      loop
    "5" -> do
      putStrLn "Exit"
    _ -> do
      putStrLn "Invalid command"
      loop
  where
    loop = usersMenu conn  
