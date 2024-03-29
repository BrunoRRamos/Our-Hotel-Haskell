{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Review (module Models.Review) where

import Data.Time.Clock
import Data.List (find)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics

data Review = Review
    {   reviewId :: Int,
        reservationId :: Int,
        rating :: Int,
        comments :: String,
        date :: UTCTime,
        userId :: String
    } deriving (Show)

instance FromRow Review where
  fromRow = Review <$> field <*> field <*> field <*> field <*> field <*> field

createReviewTable :: Connection -> IO ()
createReviewTable conn =
  execute_ conn $
    Query $
      "CREATE TABLE IF NOT EXISTS reviews (\
      \review_id INTEGER PRIMARY KEY AUTOINCREMENT,\
      \reservation_id INTEGER,\
      \rating INTEGER,\
      \comments TEXT,\
      \date TEXT,\
      \user_id TEXT\
      \)"

insertReview :: Connection -> Review -> IO ()
insertReview conn review =
  execute conn "INSERT INTO reviews (reservation_id, rating, comments, date, user_id) VALUES (?, ?, ?, ?, ?)"
    (reservationId review, rating review, comments review, show $ date review, userId review)

getAllReviews :: Connection -> IO [Review]
getAllReviews conn = query_ conn "SELECT * FROM reviews"