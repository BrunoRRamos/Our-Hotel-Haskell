module Util.StayReview
    ( generateStayReview
    ) where

import Data.Time.Clock
import Database.SQLite.Simple
import Models.Review (Review(..), insertReview)

data StayReview = StayReview
    {   _reservationId :: Int,
        _rating :: Int,
        _comments :: String,
        _date :: UTCTime
    }
    deriving (Show)

generateStayReview :: Connection -> Int -> IO ()
generateStayReview conn reservation = do
    putStrLn "Please provide your rating (from 1 to 5): "
    ratingInput <- getLine
    let rating = read ratingInput :: Int
    putStrLn "Please provide your comments: "
    comments <- getLine
    currentTime <- getCurrentTime
    insertReview conn $ Review
        {   reservationId = reservation,
            rating = rating,
            comments = comments,
            date = currentTime
        }
    putStrLn "Review added successfully!"
