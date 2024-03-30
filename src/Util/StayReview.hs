module StayReview
    ( generateStayReview
    ) where

import Data.Time.Clock
import Database.SQLite.Simple
import Models.User (User(..))
import Models.Reservation (Reservation(..))

data StayReview = StayReview
    {   _reservationId :: Int,
        _rating :: Int,
        _comments :: String,
        _date :: UTCTime,
        _userId :: String
    }
    deriving (Show)

generateStayReview :: Connection -> Reservation -> User -> IO StayReview
generateStayReview reservation user = do
    putStrLn "Please provide your rating (from 1 to 5): "
    ratingInput <- getLine
    let rating = read ratingInput :: Int
    putStrLn "Please provide your comments: "
    comments <- getLine
    currentTime <- getCurrentTime
    execute conn "INSERT INTO reviews (reservation_id, rating, comments, date, user_id) VALUES (?, ?, ?, ?, ?)"
        (_id reservation, rating, comments, show currentTime, _email user)
    putStrLn "Review added successfully!"