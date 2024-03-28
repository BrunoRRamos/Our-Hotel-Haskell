{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}


module Util.Hospede (requestRoomService) where

import Models.Service (Service(..), ServiceType(..), createService)
import Database.SQLite.Simple

-- Função para requisitar serviço de quarto
requestRoomService :: Connection -> Int -> Double -> ServiceType -> String -> IO ()
requestRoomService conn reservationId price serviceType description = do
    let service = Service { _price = price, _type = serviceType, _description = description, _reservationId = reservationId }
    createService conn service
