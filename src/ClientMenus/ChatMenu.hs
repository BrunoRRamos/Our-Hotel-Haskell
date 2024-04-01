{-# OPTIONS_GHC -Wno-missing-fields #-}
module ClientMenus.ChatMenu (chatMenu) where

import Database.SQLite.Simple (Connection)
import Models.Message (Message (..), createMessage, getMessageRecipient, getSenderMessages)
import Util.IO (askForInput)
import System.Exit (die)

chatMenu :: Connection -> [String] -> IO ()
chatMenu conn args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  View recived messages"
  putStrLn "2.  View sent messages"
  putStrLn "3.  Whrite a new message"
  putStrLn "4.  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      putStrLn "\nInsert your E-mail:"
      userEmail <- getLine
      messages <- getMessageRecipient conn userEmail
      print messages
    "2" -> do
      putStrLn "\nInsert your E-mail:"
      userEmail <- getLine
      messages <- getSenderMessages conn userEmail
      print messages
    "3" -> do
      putStrLn "\nInsert your E-mail:"
      senderEmail <- getLine

      putStrLn "\nInsert the recipient E-mail:"
      recipientEmail <- getLine

      putStrLn "\nInsert the message:"
      message <- getLine

      createMessage
        conn
        Message
          { _sender_email = senderEmail,
            _recipient_email = recipientEmail,
            _message = message
          }
      putStrLn "Message Sent!"
    "4" -> do
      die "Goodbye!"
    _ -> do
      putStrLn "\n\nInvalid command. Please try again."
      chatMenu conn args