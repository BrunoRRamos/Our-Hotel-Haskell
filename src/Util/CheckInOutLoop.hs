{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Util.CheckInOutLoop (checkInOutLoop) where

import Control.Exception (try, SomeException)
import Models.Reservation (checkIn, checkOut)
import Database (startDb)
import System.Exit (die)
import Database.SQLite.Simple (Connection)

checkInInput :: Connection -> IO ()
checkInInput conn = do
    putStrLn "\nInsert your Reservation ID: "
    result <- try $ do
        reservationId <- readLn :: IO Int
        checkIn conn reservationId

    case result of
        Left (ex :: SomeException) ->  do
            putStrLn "Error: Invalid reservation ID or No reservation found.\nCheck your reservation ID this must be a number."
            checkInInput conn
        Right _ -> print ""
   
checkOutInput :: Connection -> IO ()
checkOutInput conn = do
    putStrLn "\nInsert your Reservation ID: "
    result <- try $ do
        reservationId <- readLn :: IO Int
        checkOut conn reservationId
    
    case result of
        Left (ex :: SomeException) ->  do
            putStrLn "Error: Invalid reservation ID or No reservation found.\nCheck your reservation ID this must be a number."
            checkInInput conn
        Right _ -> print ""

checkInOutLoop :: [String] -> IO ()
checkInOutLoop args = do
    conn <- startDb
    putStrLn "\nAvailable commands:"
    putStrLn "1.  Check-In"
    putStrLn "2.  Check-Out"
    putStrLn "3.  exit - Quit the program"
    putStrLn "\nEnter a command: "
    cmd <- getLine
    let nextArgs = words cmd
    case head nextArgs of
        "1" -> do
            checkInInput conn
        
        "2" -> do
            checkOutInput conn
        
        "3" -> do
            die "Goodbye!"

        _ -> do
            putStrLn "Invalid command. Please try again."
            checkInOutLoop args
       

