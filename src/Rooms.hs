module Rooms
  ( roomsLoop,
  )
where
  
import Database.SQLite.Simple
import Models.Service (Service (..), ServiceType (..), createService, getService, getAllServices, printService, calculateTotalPrice)
import Database (startDb)
import Control.Monad (when)

roomsLoop :: [String] -> IO ()
roomsLoop args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  list_rooms"
  putStrLn "2.  get_room"
  putStrLn "3.  get_service_by_id"
  putStrLn "4.  sum_price_by_idReservation"
  putStrLn "5.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "list_hotels" -> do
      roomsLoop args
    "get_hotel" -> do
      roomsLoop args
    "3" -> do
      conn <- startDb
      putStrLn "Enter service ID:"
      serviceId <- getLine
      let sid = read serviceId :: Int
      service <- getService conn sid
      when (service == Service 0 0 CLEANING "" 0) $ putStrLn "Service not found"
      printService service
      roomsLoop args
    "4" -> do
      conn <- startDb
      putStrLn "Enter reservation ID:"
      reservationId <- readLn :: IO Int
      totalPrice <- calculateTotalPrice conn reservationId
      case totalPrice of
        Just price -> putStrLn $ "Total price for reservation " ++ show reservationId ++ ": " ++ show price
        Nothing -> putStrLn "Reservation not found"
      roomsLoop args
    "5" -> putStrLn "Goodbye!"

    _ -> do
      putStrLn "Invalid command. Please try again."
      roomsLoop args
