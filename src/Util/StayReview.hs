module Util.StayReview
    ( generateStayReview
    ) where

import Data.Time.Clock
import Database.SQLite.Simple
import Models.User (User(..))
import Models.Reservation (Reservation(..))
import Models.Review (Review(..), insertReview)

data StayReview = StayReview
    {   _reservationId :: Int,
        _rating :: Int,
        _comments :: String,
        _date :: UTCTime,
        _userId :: String
    }
    deriving (Show)

generateStayReview :: Connection -> Reservation -> User -> IO ()
generateStayReview conn reservation user = do
    putStrLn "Please provide your rating (from 1 to 5): "
    ratingInput <- getLine
    let rating = read ratingInput :: Int
    putStrLn "Please provide your comments: "
    comments <- getLine
    currentTime <- getCurrentTime
    insertReview conn $ Review
        {   reviewId = 0,
            reservationId = _id reservation,
            rating = rating,
            comments = comments,
            date = currentTime,
            userId = _email user
        }
    putStrLn "Review added successfully!"