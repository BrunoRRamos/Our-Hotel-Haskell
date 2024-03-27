{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Util.LoginLoop
  ( loginLoop,
  )
where

import Database.SQLite.Simple
import Models.User (Role (..), User (..), createUser, getAllUsers, getUser)
import System.Exit (die)
import Util.Login (login)

loginError conn = do
  print "E-mail or Password Invalid, Try agin"
  loginLoop conn

loginLoop :: Connection -> IO (Maybe User)
loginLoop conn = do
  users <- getAllUsers conn
  putStrLn "\nAvailable commands:"
  putStrLn "1.  Login"
  putStrLn "2.  Register"
  putStrLn "3.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      putStrLn "\nInsert your Email: "
      email <- getLine
      putStrLn "\nInsert your Password: "
      password <- getLine
      if login email password users
        then do
          print "Login Successful"
          getUser conn email
        else loginError conn
    "2" -> do
      putStrLn "\nInsert your First Name: "
      firstName <- getLine

      putStrLn "\nInsert your Last Name: "
      lastName <- getLine

      putStrLn "\nInsert your E-mail: "
      email <- getLine

      putStrLn "\nInsert your Password: "
      password <- getLine

      createUser
        conn
        User
          { _firstName = firstName,
            _lastName = lastName,
            _email = email,
            _password = password,
            _isActive = True,
            _role = CLIENT
          }
      getUser conn email
    "3" -> do
      die "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      loginLoop conn