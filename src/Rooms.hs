module Rooms
  ( roomsLoop,
  )
where
  
import Database.SQLite.Simple
import Models.Service (Service (..), ServiceType (..), createService, getService, getAllServices, calculateTotalPrice)
import Database (startDb)

roomsLoop :: [String] -> IO ()
roomsLoop args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  List all rooms"
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
      serviceId <- readLn :: IO Int
      maybeService <- getService conn serviceId
      case maybeService of
        Just service -> do
          putStrLn $ show service
          roomsLoop args
        Nothing -> do
          putStrLn "Service not found"
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
