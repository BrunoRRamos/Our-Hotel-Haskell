module Util.LoginLoop
    ( loginLoop
    ) where

import Util.Login (login)

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
  putStrLn "\nAvailable commands:"
  putStrLn "  login"
  putStrLn "  register"
  putStrLn "  back"
  putStrLn "  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "login" -> do
      putStrLn "\nInsert your Email: "
      email <- getLine
      putStrLn "\nInsert your Password: "
      password <- getLine
      if login email password ["test", "senha"] then userRedirect else loginError args
    "register" -> do
      loginLoop args
    "exit" -> putStrLn "Goodbye!"
    _ -> do
        putStrLn "Invalid command. Please try again."
        loginLoop args