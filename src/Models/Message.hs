{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Models.Message (module Models.Message) where

import Database.SQLite.Simple
import GHC.Generics

data Message =  Message
    { _id :: Int,
      _sender_email :: String,
      _recipient_email :: String,
      _message :: String
    }
    deriving (Show, Generic)

instance FromRow Message

createMessageTable :: Connection -> IO ()
createMessageTable conn =
   execute_
     conn
     "CREATE TABLE IF NOT EXISTS message (\
     \id INTEGER PRIMARY KEY AUTOINCREMENT,\
     \sender_email TEXT NOT NULL,\
     \recipient_email TEXT NOT NULL,\
     \message TEXT NOT NULL,\
     \FOREIGN KEY (sender_email) REFERENCES user(email),\
     \FOREIGN KEY (recipient_email) REFERENCES user(email))"

createMessage :: Connection -> Message -> IO ()
createMessage conn message = do
    execute
     conn
     "INSERT INTO message (sender_email, recipient_email, message) VALUES (?, ?, ?)"
     ( _sender_email message,
       _recipient_email message,
       _message message
     )

getAllMessages :: Connection -> IO [Message]
getAllMessages conn = query_ conn "SELECT * FROM message" :: IO [Message]

joinEqualSender :: [Message] -> String -> [Message]
joinEqualSender [] _ = []
joinEqualSender (h : t) sender_email =
    if _sender_email h == sender_email
        then h : joinEqualSender t sender_email
        else joinEqualSender t sender_email

joinEqualRecipient :: [Message] -> String -> [Message]
joinEqualRecipient [] _ = []
joinEqualRecipient (h : t) recipient_email =
    if _recipient_email h == recipient_email
        then h : joinEqualSender t recipient_email
        else joinEqualSender t recipient_email

getSenderMessages :: Connection -> String -> IO [Message]
getSenderMessages conn sender_email = do
    messages <- getAllMessages conn
    return (joinEqualSender messages sender_email)

getMessageRecipient :: Connection -> String -> IO [Message]
getMessageRecipient conn recipient_email = do
    messages <- getAllMessages conn
    return (joinEqualRecipient messages recipient_email)
