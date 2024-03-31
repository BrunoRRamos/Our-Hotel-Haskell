{-# OPTIONS_GHC -Wno-missing-fields #-}

module MainMenu
  ( mainMenu,
  )
where

import Util.Database (startDb)
import Util.IO (clearScreen)
import LoginMenu (loginMenu)
import Models.User (Role(ADMIN, CLIENT), User (_role))
import ClientMenu (clientMenu)
import AdminMenu (adminMenu)

mainMenu :: [String] -> IO ()
mainMenu args = do
  clearScreen
  conn <- startDb
  loggedUser <- loginMenu conn
  let user = case loggedUser of
        Just u -> u
        Nothing -> error "User not found"

  case _role user of
    ADMIN -> adminMenu conn user args
    CLIENT -> clientMenu conn user args
