module Util.LoginLoop
    ( loginLoop
    ) where

import Util.Login (login)
import Database (startDb)
import Models.User (Role (..), User (..), getAllUsers)
import System.Exit (die)

userRedirect :: IO ()
userRedirect = do
    print "Login Sucesfull"
    {-Redirecina para pagian de Hospede ou Gerente-}

loginError :: [String] -> IO ()
loginError args = do
    print "Login Error"
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
      putStrLn "\nInsert your Email: "
      email <- getLine
      putStrLn "\nInsert your Password: "
      password <- getLine
      if login email password users then userRedirect else loginError args
    "2" -> do
      loginLoop args
    "3" -> do
      die "Goodbye!"
    _ -> do
        putStrLn "Invalid command. Please try again."
        loginLoop args