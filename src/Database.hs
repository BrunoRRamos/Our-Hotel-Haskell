{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Database where
import Database.Beam
    ( Generic,
      defaultDbSettings,
      Database,
      DatabaseSettings,
      TableEntity )
import Schema

data HotelDb f = HotelDb
                      { _users :: f (TableEntity UserT) }
                        deriving (Generic, Database be)

hotelDb :: DatabaseSettings be HotelDb
hotelDb = defaultDbSettings
