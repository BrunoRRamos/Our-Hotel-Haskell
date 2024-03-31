{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Util.ReviewHotel (averageRating, bestRooms, clientsToday, lastMonthReservations, roomAvailability, totalRevenue, thisMonthReservations) where

import Data.Maybe (fromMaybe)
import Data.Time (UTCTime (utctDay), getCurrentTime, diffDays, addGregorianMonthsClip)
import Database.SQLite.Simple (Connection)
import Models.Reservation (Reservation (_rating, _userId, _roomId, _end, _start), getAllReservations, overlap, getAvailableRooms)
import Models.User (User (..), getAllUsers)
import Models.Room (Room (_id, _dailyRate), getAllRooms)
import Data.List (sortBy)

clientsToday :: Connection -> IO [User]
clientsToday conn = do
  reservations <- getAllReservations conn
  today <- utctDay <$> getCurrentTime
  let reservationsToday = filter (overlap today today) reservations
  users <- getAllUsers conn
  return $ filter (\user -> any (\reservation -> _userId reservation == _email user) reservationsToday) users

roomAvailability :: Connection -> IO [Room]
roomAvailability conn = do
  today <- utctDay <$> getCurrentTime
  getAvailableRooms conn today today

averageRating :: Connection -> IO Int
averageRating conn = do
  reservations <- getAllReservations conn
  let ratings = map (fromMaybe 0 . _rating) reservations
  case length ratings of
    0 -> return 0
    _ -> return $ sum ratings `div` length ratings 

totalRevenue :: Connection -> IO Double
totalRevenue conn = do
  reservations <- getAllReservations conn
  rooms <- getAllRooms conn
  let roomPrices = map (\room -> (_id room, _dailyRate room)) rooms
  let revenues = map (\reservation -> 
                        let days = fromIntegral $ diffDays (_end reservation) (_start reservation)
                            price = fromMaybe 0 (lookup (_roomId reservation) roomPrices)
                        in  days * price
                      ) reservations
  return $ sumDouble revenues
  where 
    sumDouble :: [Double] -> Double
    sumDouble [] = 0 
    sumDouble (x:xs) = x + sumDouble xs

bestRooms :: Connection -> IO [Room]
bestRooms conn = do
  rooms <- getAllRooms conn
  reservations <- getAllReservations conn
  let topRooms = sortBy (\room1 room2 -> compare (length $ filter (\reservation -> _roomId reservation == _id room2) reservations) (length $ filter (\reservation -> _roomId reservation == _id room1) reservations)) rooms
  return $ take 5 topRooms

lastMonthReservations :: Connection -> IO [Reservation]
lastMonthReservations conn = do
  currentDay <- utctDay <$> getCurrentTime
  let lastMonthDay = addGregorianMonthsClip (-1) currentDay
  reservations <- getAllReservations conn
  let _lastMonthReservations = filter (\reservation -> 
                                        diffDays currentDay (_start reservation) >= 0 && 
                                            diffDays currentDay (_start reservation) <= 30
                                      ) reservations
  return _lastMonthReservations

thisMonthReservations :: Connection -> IO [Reservation]
thisMonthReservations conn = do
  currentDay <- utctDay <$> getCurrentTime
  let nextMonthDay = addGregorianMonthsClip 1 currentDay
  reservations <- getAllReservations conn
  let _thisMonthReservations = filter (\reservation -> 
                                        diffDays currentDay (_start reservation) >= 0 && 
                                            diffDays currentDay (_start reservation) <= 30
                                      ) reservations
  return _thisMonthReservations
