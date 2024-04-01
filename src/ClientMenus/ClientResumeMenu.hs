module ClientMenus.ClientResumeMenu (clientResumeMenu) where

import Database.SQLite.Simple (Connection)
import Models.Reservation (reservationResume)

clientResumeMenu :: Connection -> IO ()
clientResumeMenu conn = do
    putStrLn "\nInsert your Reservation ID: "
    reservationId <- readLn :: IO Int
    reservationResume conn reservationId