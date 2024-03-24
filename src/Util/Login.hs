{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Util.Login (login) where
import Models.User (User (..))

verifyEmail :: String -> [User] -> Bool
verifyEmail email [] = False
verifyEmail email (h : t) = (_email h == email) || verifyEmail email t

verifyPassword :: String -> [User] -> Bool
verifyPassword password [] = False
verifyPassword password (h : t) = (_password h == password) || verifyPassword password t

login :: String -> String -> [User] -> Bool
login email password usersArr = verifyEmail email usersArr && verifyPassword password usersArr
