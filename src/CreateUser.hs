{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module CreateUser where
import Database.Beam.Sqlite ( runBeamSqliteDebug )

import Database.SQLite.Simple ( open )
import Database.Beam ( insert, insertValues, runInsert )
import qualified Data.Text
import Schema (UserT (User))
import Database (hotelDb, HotelDb(_users))
createUser :: IO ()
createUser = do
    conn <- open "hotel.db"
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_users  hotelDb ) $
        insertValues [ User (Data.Text.pack "james@example.com") (Data.Text.pack "James") (Data.Text.pack "Smith") (Data.Text.pack "b4cc344d25a2efe540adbf2678e2304c") ]
             
