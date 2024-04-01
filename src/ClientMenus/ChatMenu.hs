{-# OPTIONS_GHC -Wno-missing-fields #-}
module ClientMenus.ChatMenu (chatMenu) where

import Database.SQLite.Simple (Connection)
import Models.Message (Message (..), createMessage, getMessageRecipient, getSenderMessages)
import Data.List (null)
import System.Exit (die)

chatMenu :: Connection -> [String] -> IO ()
chatMenu conn args = do
  putStrLn "\nAvailable commands:"
  putStrLn "1.  View recived messages"
  putStrLn "2.  View sent messages"
  putStrLn "3.  Whrite a new message"
  putStrLn "4.  Go back"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "1" -> do
      putStrLn "\nInsert your E-mail:"
      userEmail <- getLine
      messages <- getMessageRecipient conn userEmail
      if null messages then print "You haven't received messages yet!" else print (show messages)
      chatMenu conn args
    "2" -> do
      putStrLn "\nInsert your E-mail:"
      userEmail <- getLine
      messages <- getSenderMessages conn userEmail
      if null messages then print "You haven't received messages yet!" else print (show messages)
      chatMenu conn args
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
      chatMenu conn args
    "4" -> do
      return ()
    _ -> do
      putStrLn "\n\nInvalid command. Please try again."
      chatMenu conn args