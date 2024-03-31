{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Service (module Models.Service) where

import Data.List (find)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics

data ServiceType = CLEANING | MEAL deriving (Show, Eq)

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
    _description :: String
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
    \type TEXT CHECK(type IN ('CLEANING', 'MEAL') NOT NULL),\
    \description TEXT NOT NULL)"

createService :: Connection -> Service -> IO Service
createService conn service = do
  execute
    conn
    "INSERT INTO service (price, type, description) VALUES (?, ?, ?)"
    (_price service, show $ _type service, _description service)
  serviceId <- lastInsertRowId conn
  return $ service {_id = fromIntegral serviceId}

updateService :: Connection -> Int -> Service -> IO ()
updateService conn serviceId service = do
  execute
    conn
    "UPDATE service SET price = ?, type = ?, description = ? WHERE id = ?"
    (_price service, show $ _type service, _description service, serviceId)

deleteService :: Connection -> Int -> IO ()
deleteService conn serviceId = do
  execute conn "DELETE FROM service WHERE id = ?" (Only serviceId)

getService :: Connection -> Int -> IO (Maybe Service)
getService conn serviceId = do
  services <- query_ conn "SELECT * FROM service" :: IO [Service]
  return $ find (\service -> _id service == serviceId) services

getAllServices :: Connection -> IO [Service]
getAllServices conn = query_ conn "SELECT * FROM service" :: IO [Service]

getServiceByType :: Connection -> ServiceType -> IO [Service]
getServiceByType conn serviceType = do
  services <- getAllServices conn
  return $ filter (\service -> _type service == serviceType) services