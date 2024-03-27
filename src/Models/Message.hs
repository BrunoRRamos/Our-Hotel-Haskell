{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Models.Message (module Models.Message) where

import Data.List (find)
import Database.SQLite.Simple
import GHC.Generics

data Message =  Message
    { _id :: Int,
      _senderId :: Int,
      _recipientId :: Int,
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
     \sender_id INTEGER NOT NULL,\
     \recipient_id INTEGER NOT NULL,\
     \message TEXT NOT NULL,\
     \FOREIGN KEY (sender_id) REFERENCES user(id),\
     \FOREIGN KEY (recipient_id) REFERENCES user(id))"

createMessage :: Connection -> Int -> Int -> Message -> IO ()
createMessage conn _senderId _recipientId _message = do
    execute
     conn
     "INSERT INTO message (sender_id, recipient_id, message) VALUES (?, ?, ?)"
     ( _senderId,
       _recipientId,
       _message
     )

getAllMessages :: Connection -> IO [Message]
getAllMessages conn = query_ conn "SELECT * FROM message" :: IO [Message]



{-
joinEqualSender :: [Message] -> Int -> [Message]
joinEqualSender [] _ = []
joinEqualSender (h : t) senderId =
    if _senderId h == senderId
        then h : joinEqualSender t senderId
        else joinEqualSender t senderId

getSenderMessages :: Connection -> Int -> IO [Message]
getSenderMessages conn senderId = do
    messages <- getAllMessages conn
    return (joinEqualSender  messages senderId)

getMessageRecipient :: Connection -> Int -> IO [Message]
getMessageRecipient conn recipientId = do
    messages <- getAllMessages conn
-}