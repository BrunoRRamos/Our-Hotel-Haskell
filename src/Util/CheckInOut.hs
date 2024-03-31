{-# LANGUAGE ScopedTypeVariables #-}

module Util.CheckInOut(checkInInput,checkOutInput) where 

import Database.SQLite.Simple (Connection)
import Control.Exception (try, SomeException )
import Models.Reservation (checkIn, checkOut)

checkInInput :: Connection -> IO ()
checkInInput conn = do
    putStrLn "\nInsert your Reservation ID: "
    result <- try $ do
        reservationId <- readLn :: IO Int
        checkIn conn reservationId

    case result of
        Left (_ :: SomeException) ->  do
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
        Left (_ :: SomeException) ->  do
            putStrLn "Error: Invalid reservation ID or No reservation found.\nCheck your reservation ID this must be a number."
            checkInInput conn
        Right _ -> print ""