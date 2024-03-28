module Util.HospedeLoop
    ( hospedeLoop
    ) where

import Util.Hospede (requestRoomService)
import Database (startDb)
import Models.Service (ServiceType (..))
import System.Exit (die)
import Control.Exception

hospedeLoop :: [String] -> IO ()
hospedeLoop args = do
  conn <- startDb
  putStrLn "\nAvailable commands:"
  putStrLn "1. Request room cleaning"
  putStrLn "2. Request meal service"
  putStrLn "3. Exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      putStrLn "\nRequesting room cleaning..."
      result <- try $ requestRoomService conn 1 50 CLEANING "Clean the room" :: IO (Either SomeException ())
      case result of
        Left _ -> putStrLn "Failed to request room cleaning."
        Right _ -> putStrLn "Room cleaning requested successfully."
      hospedeLoop args

    "2" -> do
      putStrLn "\nRequesting meal service..."
      result <- try $ requestRoomService conn 1 50 MEAL "Deliver a yummy meal" :: IO (Either SomeException ())
      case result of
        Left _ -> putStrLn "Failed to request meal service."
        Right _ -> putStrLn "Meal service requested successfully."
      hospedeLoop args

    "3" -> die "Goodbye!"

