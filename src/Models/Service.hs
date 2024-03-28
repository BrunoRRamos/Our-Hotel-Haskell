{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Service (module Models.Service) where

import Data.List (find)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics
import Data.Maybe (listToMaybe)

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
  deriving (Generic)

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


getService :: Connection -> Int -> IO (Maybe Service)
getService conn serviceId = do
  services <- query_ conn "SELECT * FROM service" :: IO [Service]
  let maybeService = find (\service -> _id service == serviceId) services
  return maybeService


getAllServices :: Connection -> IO [Service]
getAllServices conn = query_ conn "SELECT * FROM service" :: IO [Service]

instance Show Service where
    show service =
        "Service ID: " ++ show (_id service) ++ "\n" ++
        "Price: " ++ show (_price service) ++ "\n" ++
        "Type: " ++ show (_type service) ++ "\n" ++
        "Description: " ++ _description service ++ "\n" ++
        "Reservation ID: " ++ show (_reservationId service) ++ "\n"


calculateTotalPrice :: Connection -> Int -> IO (Maybe Double)
calculateTotalPrice conn reservationId = do
  allServices <- getAllServices conn
  let servicesForReservation = filter (\service -> _reservationId service == reservationId) allServices
  let totalPrice = sum $ map _price servicesForReservation
  if null servicesForReservation
    then return Nothing
    else return $ Just totalPrice


instance Eq Service where
    (Service id1 _ _ _ _) == (Service id2 _ _ _ _) = id1 == id2

    