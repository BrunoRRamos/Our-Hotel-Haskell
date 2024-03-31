{-# OPTIONS_GHC -Wno-missing-fields #-}

module Util.Service (addService, editService, removeService) where

import Database.SQLite.Simple (Connection)
import Models.Service (Service (..), ServiceType (..), createService, deleteService, getService, updateService)
import Text.Read (readMaybe)
import Util.IO (askForInput, askForInputWithDefault)

serviceForm :: Maybe Service -> IO Service
serviceForm defaultService = do
  case defaultService of
    Just service -> do
      description <- askForInputWithDefault "Enter description: " (return . Just) (_description service)
      price <- askForInputWithDefault "Enter price: " (return . (readMaybe :: String -> Maybe Double)) (_price service)
      serviceType <- askForInputWithDefault "Enter service type - (C)leaning or (M)eal: " parseServiceType (_type service)

      return Service {_price = price, _description = description, _type = serviceType}
    Nothing -> do
      description <- askForInput "Enter description: " (return . Just)
      price <- askForInput "Enter price: " (return . (readMaybe :: String -> Maybe Double))
      serviceType <- askForInput "Enter service type - (C)leaning or (M)eal: " parseServiceType

      return Service {_price = price, _description = description, _type = serviceType}

parseServiceType :: String -> IO (Maybe ServiceType)
parseServiceType str =
  case str of
    "C" -> return $ Just CLEANING
    "M" -> return $ Just MEAL
    _ -> putStrLn "Invalid service type" >> return Nothing

addService :: Connection -> IO Service
addService conn = do
  service <- serviceForm Nothing
  createService conn service

editService :: Connection -> IO ()
editService conn = do
  service <- getServiceById conn
  let serviceId = _id service
  newService <- serviceForm (Just service)
  updateService conn serviceId (newService {_id = serviceId})

removeService :: Connection -> IO ()
removeService conn = do
  service <- getServiceById conn
  let serviceId = _id service
  deleteService conn serviceId

getServiceById :: Connection -> IO Service
getServiceById conn = do
  askForInput
    "\nEnter service id: "
    ( \str -> do
        let input = (readMaybe str :: Maybe Int)
        case input of
          Nothing -> putStrLn "Invalid service id! Please try again" >> return Nothing
          Just i -> do
            res <- getService conn i
            maybe (putStrLn "Service not found! Please try again" >> return Nothing) (return . Just) res
    )
