{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use /=" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

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
    _block_reason :: Maybe String,
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
    \block_reason TEXT,\
    \role TEXT CHECK(role IN ('ADMIN', 'CLIENT')) NOT NULL)"

createUser :: Connection -> User -> IO ()
createUser conn user =
  execute
    conn
    "INSERT INTO user (email, first_name, last_name, password, is_active, role) VALUES (?, ?, ?, ?, ?, ?)"
    (_email user, _firstName user, _lastName user, _password user, _isActive user, show $ _role user)

getUser :: Connection -> String -> IO (Maybe User)
getUser conn email = do
  users <- query_ conn "SELECT * FROM user" :: IO [User]
  return $ find (\user -> _email user == email) users

getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn "SELECT * FROM user" :: IO [User]

blockClient :: Connection -> String -> String -> IO ()
blockClient conn clientId reason = do
  execute conn "UPDATE user SET is_active = ?, block_reason = ? WHERE email =?" (0 :: Int, Just reason, clientId)

verifyEmailIsDisp :: [User] -> String -> Bool
verifyEmailIsDisp [] email = True
verifyEmailIsDisp (h : t) email = not (_email h == email) && verifyEmailIsDisp t email