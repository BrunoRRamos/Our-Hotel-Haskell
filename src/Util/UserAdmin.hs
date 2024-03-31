{-# OPTIONS_GHC -Wno-missing-fields #-}
module Util.UserAdmin(createUserAdmin, deleteUserAdmin) where
import Database.SQLite.Simple (Connection)
import Models.User (User (..), Role (ADMIN), getAllUsers, createUser, deleteUser)
import Util.IO (askForInput)


createUserAdmin :: Connection -> IO ()
createUserAdmin conn = do
    firstName <- askForInput "Insert your First Name: " returnString
    lastName <- askForInput "Insert your Last Name: " returnString
    email <- askForInput "Insert your Email: " validateEmail
    password <- askForInput "Insert your Password: " returnString

    createUser conn User { _firstName = firstName, _lastName = lastName, _email = email, _password = password, _isActive = True, _role = ADMIN}
    
    putStrLn "Admin user created successfully!"
    
    where
        validateEmail :: String -> IO (Maybe String)
        validateEmail email = do
          users <- getAllUsers conn
          if any (\u -> email == _email u) users
            then do
              putStrLn "Email already in use. Please try again."
              return Nothing
            else return $ Just email
        returnString :: String -> IO (Maybe String)
        returnString s = return $ Just s

deleteUserAdmin :: Connection -> IO ()
deleteUserAdmin conn = do
    userEmail <- askForInput "Insert the email of the user you want to delete: " validateUserEmail 
    deleteUser conn userEmail
    putStrLn "User deleted successfully!"
    where
        validateUserEmail :: String -> IO (Maybe String)
        validateUserEmail email = do
            users <- getAllUsers conn
            if any (\u -> email == _email u) users
                then return $ Just email
                else do
                    putStrLn "User not found. Please try again."
                    return Nothing