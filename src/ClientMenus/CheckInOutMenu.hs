{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module ClientMenus.CheckInOutMenu (checkInOutMenu) where


import System.Exit (die)
import Database.SQLite.Simple (Connection)
import Util.CheckInOut (checkInInput, checkOutInput)


checkInOutMenu :: Connection -> [String] -> IO ()
checkInOutMenu conn args = do
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
            checkInOutMenu conn args
       

