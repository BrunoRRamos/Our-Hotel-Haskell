{-# LANGUAGE OverloadedStrings #-}

module Util.Hospede (requestRoomService) where

import Models.Service (Service(..), ServiceType(..), createService)
import Models.Reservation (getReservation, Reservation(..))
import Database.SQLite.Simple

-- Função para requisitar serviço de quarto
requestRoomService :: Connection -> Int -> Double -> ServiceType -> String -> IO ()
requestRoomService conn reservationId price serviceType description = do
    maybeReservation <- getReservation conn reservationId
    case maybeReservation of
        Just reservation -> do
            if _serviceStatus reservation /= "FREE"
                then putStrLn "Service status must be FREE to create a service."
                else do
                    let service = Service { _price = price, _type = serviceType, _description = description, _reservationId = reservationId }
                    createService conn service
        Nothing -> putStrLn $ "Reservation with ID " ++ show reservationId ++ " not found."
