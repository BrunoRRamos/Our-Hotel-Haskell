{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User (module Models.User) where

import Data.List (find)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics

data Role = ADMIN | CLIENT
  deriving (Show, Eq)

instance FromField Role where
  fromField :: FieldParser Role
  fromField f = do
    _field <- (fromField :: FieldParser String) f
    case _field of
      "ADMIN" -> return ADMIN
      "CLIENT" -> return CLIENT
      _ -> returnError ConversionFailed f "Unknown role"

data User = User
  { _email :: String,
    _firstName :: String,
    _lastName :: String,
    _password :: String,
    _isActive :: Bool,
    _role :: Role
  }
  deriving (Show, Generic)

instance FromRow User

createUserTable :: Connection -> IO ()
createUserTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS user (\
    \email TEXT PRIMARY KEY,\
    \first_name TEXT NOT NULL,\
    \last_name TEXT NOT NULL,\
    \password TEXT NOT NULL,\
    \is_active BOOLEAN NOT NULL DEFAULT 1,\
    \role TEXT CHECK(role IN ('ADMIN', 'CLIENT')) NOT NULL)"

createUser :: Connection -> User -> IO ()
createUser conn user =
  execute
    conn
    "INSERT INTO user (email, first_name, last_name, password, is_active, role) VALUES (?, ?, ?, ?, ?, ?)"
    (_email user, _firstName user, _lastName user, _password user, _isActive user, show $ _role user)

getUser :: Connection -> String -> IO User
getUser conn email = do
  users <- query_ conn "SELECT * FROM user" :: IO [User]
  let _user = find (\user -> _email user == email) users
  case _user of
    Just user -> return user
    Nothing -> error "User not found"