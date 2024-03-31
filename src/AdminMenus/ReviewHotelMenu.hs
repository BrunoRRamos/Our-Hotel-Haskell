module AdminMenus.ReviewHotelMenu(hotelReviewMenu) where

import Database.SQLite.Simple (Connection)
import Util.ReviewHotel (clientsToday, roomAvailability, averageRating, bestRooms, totalRevenue, lastMonthReservations, thisMonthReservations)

hotelReviewMenu :: Connection -> IO ()
hotelReviewMenu conn = do
    putStrLn "Review Hotel Menu"
    putStrLn "1. Total clients in reservations today"
    putStrLn "2. Room availability"
    putStrLn "3. Average rating"
    putStrLn "4. Total revenue"
    putStrLn "5. Best rooms"
    putStrLn "6. Last month reservations"
    putStrLn "7. This month reservations"
    putStrLn "8. Exit"
    putStrLn "Enter your choice: "
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Total clients"
            clientsToday conn >>= print
            hotelReviewMenu conn
        "2" -> do
            putStrLn "Room availability"
            roomAvailability conn >>= print
            hotelReviewMenu conn
        "3" -> do
            putStrLn "Average rating"
            averageRating conn >>= print
            hotelReviewMenu conn
        "4" -> do
            putStrLn "Total revenue"
            totalRevenue conn >>= print
            hotelReviewMenu conn
        "5" -> do
            putStrLn "Best rooms"
            bestRooms conn >>= print
            hotelReviewMenu conn
        "6" -> do
            putStrLn "Last month reservations"
            lastMonthReservations conn >>= print
            hotelReviewMenu conn
        "7" -> do
            putStrLn "This month reservations"
            thisMonthReservations conn >>= print
            hotelReviewMenu conn
        "8" -> return ()
        _ -> do
            putStrLn "Invalid choice. Please try again."
            hotelReviewMenu conn
