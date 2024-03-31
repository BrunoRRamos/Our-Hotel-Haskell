module ClientMenus.RoomsMenu
  ( roomsMenu,
  )
where
  
import Models.Service (getService, calculateTotalPrice)
import Database.SQLite.Simple (Connection)

roomsMenu :: Connection -> [String] -> IO ()
roomsMenu conn args = do
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
      roomsMenu conn args
    "get_hotel" -> do
      roomsMenu conn args
    "3" -> do
      putStrLn "Enter service ID:"
      serviceId <- readLn :: IO Int
      maybeService <- getService conn serviceId
      case maybeService of
        Just service -> do
          print service
          roomsMenu conn args
        Nothing -> do
          putStrLn "Service not found"
          roomsMenu conn args
    "4" -> do
      putStrLn "Enter reservation ID:"
      reservationId <- readLn :: IO Int
      totalPrice <- calculateTotalPrice conn reservationId
      case totalPrice of
        Just price -> putStrLn $ "Total price for reservation " ++ show reservationId ++ ": " ++ show price
        Nothing -> putStrLn "Reservation not found"
      roomsMenu conn args
    "5" -> putStrLn "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      roomsMenu conn args
