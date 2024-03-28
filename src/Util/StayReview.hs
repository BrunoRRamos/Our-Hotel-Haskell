module StayReview
    ( generateStayReview
    ) where

import Data.Time.Clock
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

generateStayReview :: Reservation -> User -> IO StayReview
generateStayReview reservation user = do
    putStrLn "Please provide your rating (from 1 to 5): "
    ratingInput <- getLine
    let rating = read ratingInput :: Int
    putStrLn "Please provide your comments: "
    comments <- getLine
    currentTime <- getCurrentTime
    return $ StayReview
        {   _reservationId = _id reservation,
            _rating = rating,
            _comments = comments,
            _date = currentTime,
            _userId = _email user
        }