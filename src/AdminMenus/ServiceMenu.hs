module AdminMenus.ServiceMenu (serviceMenu) where

import Control.Exception (try)
import Control.Monad (void)
import Database.SQLite.Simple
import Models.Service (Service)
import Util.IO (OperationCancelledException, clearScreen, pressEnter)
import Util.Service (addService, editService, removeService)

serviceMenu :: Connection -> IO ()
serviceMenu conn = do
  clearScreen
  putStrLn
    "1. Create a service\n\
    \2. Edit a service\n\
    \3. Delete a service\n\
    \4. Go back"
  cmd <- getLine

  case cmd of
    "1" -> do
      result <- try (addService conn) :: IO (Either OperationCancelledException Service)
      case result of
        Left _ -> void (putStrLn "Operation cancelled!")
        Right _ -> putStrLn "Service created successfully!"

      pressEnter
      serviceMenu conn
    "2" -> do
      result <- try (editService conn) :: IO (Either OperationCancelledException ())
      case result of
        Left _ -> void (putStrLn "Operation cancelled!")
        Right _ -> putStrLn "Service updated successfully!"

      pressEnter
      serviceMenu conn
    "3" -> do
      result <- try (removeService conn) :: IO (Either OperationCancelledException ())
      case result of
        Left _ -> void (putStrLn "Operation cancelled!")
        Right _ -> putStrLn "Service deleted successfully!"

      pressEnter
      serviceMenu conn
    "4" -> return ()
    _ -> do
      print "Invalid command. Please try again"
      serviceMenu conn