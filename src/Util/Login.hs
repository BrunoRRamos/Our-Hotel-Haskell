{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Util.Login (login) where

verifyEmail :: String -> [String] -> Bool
verifyEmail email (h : t) = (h == email) || verifyEmail email t

verifyPassword :: String -> [String] -> Bool
verifyPassword password (h : t) = (h == password) || verifyPassword password t

login :: String -> String -> [String] -> Bool
login email password usersArr = verifyEmail email usersArr && verifyPassword password usersArr
