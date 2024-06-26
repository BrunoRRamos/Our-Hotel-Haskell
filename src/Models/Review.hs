{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Models.Review (module Models.Review) where

import Data.Time.Clock
import Database.SQLite.Simple
import GHC.Generics

data Review = Review
    {   reviewId :: Int,
        reservationId :: Int,
        rating :: Int,
        comments :: String,
        date :: UTCTime
    } deriving (Show, Generic)

instance FromRow Review

createReviewTable :: Connection -> IO ()
createReviewTable conn =
  execute_ conn $
    "CREATE TABLE IF NOT EXISTS reviews (\
    \review_id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \reservation_id INTEGER NOT NULL,\
    \rating INTEGER,\
    \comments TEXT,\
    \date TEXT\
    \)"

insertReview :: Connection -> Review -> IO ()
insertReview conn review =
  execute
    conn
    "INSERT INTO reviews (reservation_id, rating, comments, date) VALUES (?, ?, ?, ?)"
    (reservationId review, rating review, comments review, show $ date review)

getAllReviews :: Connection -> IO [Review]
getAllReviews conn = query_ conn "SELECT * FROM reviews"