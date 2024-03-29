module Util.LoginLoop
    ( loginLoop
    ) where

import Util.Login (login)
import Database (startDb)
import Models.User (Role (..), User (..), createUser, getAllUsers)
import System.Exit (die)
import Control.Exception

userRedirect :: IO ()
userRedirect = do
    print "Login Sucesfull"
    {-Redirecina para pagian de Hospede ou Gerente-}

loginError :: [String] -> IO ()
loginError args = do
    print "E-mail or Password Invalid, Try agin"
    loginLoop args

loginLoop :: [String] -> IO ()
loginLoop args = do
  conn <- startDb
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
      result <- try $ createUser conn User
        { _firstName = "adm",
          _lastName = "adm",
          _email = "baseADM@gmail.com",
          _password = "adm",
          _isActive = True,
          _role = ADMIN
        } :: IO (Either SomeException ())
      case result of
        Left _ -> putStr ""
        Right _ -> putStr ""

      putStrLn "\nInsert your Email: "
      email <- getLine
      putStrLn "\nInsert your Password: "
      password <- getLine
      if login email password users then userRedirect else loginError args
    "2" -> do
      putStrLn "\nInsert your First Name: "
      firstName <- getLine

      putStrLn "\nInsert your Last Name: "
      lastName <- getLine

      putStrLn "\nInsert your E-mail: "
      email <- getLine

      putStrLn "\nInsert your Password: "
      password <- getLine

      createUser conn User
        { _firstName = firstName,
          _lastName = lastName,
          _email = email,
          _password = password,
          _isActive = True,
          _role = CLIENT
        }

    "3" -> do
      die "Goodbye!"
    _ -> do
        putStrLn "Invalid command. Please try again."
        loginLoop args