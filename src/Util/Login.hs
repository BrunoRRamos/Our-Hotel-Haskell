{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Util.Login (login) where

import Models.User (User (..))

verifyEmail :: String -> [User] -> Bool
verifyEmail email = foldr (\h -> (||) (_email h == email)) False

verifyPassword :: String -> [User] -> Bool
verifyPassword password = foldr (\h -> (||) (_password h == password)) False

login :: String -> String -> [User] -> Bool
login email password usersArr = verifyEmail email usersArr && verifyPassword password usersArr
