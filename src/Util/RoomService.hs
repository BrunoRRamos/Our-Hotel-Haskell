{-# OPTIONS_GHC -Wno-missing-fields #-}

module Util.RoomService (requestRoomService) where

import Control.Monad (when)
import Data.List (find)
import Data.Maybe (isNothing)
import Database.SQLite.Simple
import Models.Reservation (Reservation (..))
import Models.RoomService (RoomService (..), createRoomService)
import Models.Service (Service (..), ServiceType (..), getServiceByType)
import Models.User (User (..))
import Text.Read (readMaybe)
import Util.IO (askForInput, clearScreen, pressEnter)
import Util.Reservation (getReservationById)

requestRoomService :: Connection -> User -> ServiceType -> IO ()
requestRoomService conn user serviceType = do
  reservationId <- validateReservation conn user
  case reservationId of
    Just _reservationId -> do
      services <- getServiceByType conn serviceType
      putStrLn "\n## Service list##"
      mapM_ (putStrLn . showService) services
      if (length services == 0)
        then do
          putStrLn "No services available!"
          pressEnter
          return ()
        else do
          service <- askForInput "\nSelect a service:" (`validateService` services)
          createRoomService
            conn
            RoomService
              { _reservationId = _reservationId,
                Models.RoomService._serviceId = Models.Service._id service
              }
    Nothing -> return ()

validateReservation :: Connection -> User -> IO (Maybe Int)
validateReservation conn user = do
  clearScreen
  reservation <- getReservationById conn

  if Models.Reservation._userId reservation /= Models.User._email user
    then do
      putStrLn "You are not allowed to request room service for this reservation!"
      pressEnter
      return Nothing
    else return $ Just (Models.Reservation._id reservation)

validateService :: String -> [Service] -> IO (Maybe Service)
validateService serviceId services = do
  let mealId = readMaybe serviceId :: Maybe Int
  case mealId of
    Just _id -> do
      let mealService = find (\service -> Models.Service._id service == _id) services
      when (isNothing mealService) $ putStrLn "Invalid meal id!"
      return mealService
    Nothing -> do
      putStrLn "Invalid meal id!"
      return Nothing

showService :: Service -> String
showService service =
  show (Models.Service._id service)
    ++ ". $"
    ++ show (_price service)
    ++ " - "
    ++ _description service