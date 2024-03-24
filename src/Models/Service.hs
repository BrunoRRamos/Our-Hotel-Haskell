{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Service (module Models.Service) where

import Data.List (find)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics

data ServiceType = CLEANING | MEAL deriving (Show)

instance FromField ServiceType where
  fromField :: FieldParser ServiceType
  fromField f = do
    _field <- (fromField :: FieldParser String) f
    case _field of
      "CLEANING" -> return CLEANING
      "MEAL" -> return MEAL
      _ -> returnError ConversionFailed f "Unknown service type"

data Service = Service
  { _id :: Int,
    _price :: Double,
    _type :: ServiceType,
    _description :: String,
    _reservationId :: Int
  }
  deriving (Generic, Show)

instance FromRow Service

createServiceTable :: Connection -> IO ()
createServiceTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS service (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \price REAL NOT NULL,\
    \type TEXT CHECK(type IN ('CLEANING', 'MEAL')) NOT NULL,\
    \description TEXT NOT NULL,\
    \reservation_id INTEGER NOT NULL,\
    \FOREIGN KEY (reservation_id) REFERENCES reservation(id))"

createService :: Connection -> Service -> IO ()
createService conn service = do
  execute
    conn
    "INSERT INTO service (reservation_id, price, type, description) VALUES (?, ?, ?, ?)"
    (_reservationId service, _price service, show $ _type service, _description service)

getService :: Connection -> Int -> IO Service
getService conn serviceId = do
  services <- query_ conn "SELECT * FROM service" :: IO [Service]
  let _service = find (\service -> _id service == serviceId) services
  case _service of
    Just service -> return service
    Nothing -> error "Service not found"