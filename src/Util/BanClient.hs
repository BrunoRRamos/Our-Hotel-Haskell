module BanClient
    ( blockClient
    ) where

import Models.User (User(..))
import Database.SQLite.Simple

blockClient :: Connection -> String -> String -> IO ()
blockClient conn clientId reason = do
    result <- query conn "SELECT * FROM users WHERE id = ?" (Only clientId) :: IO [(String, String, Bool, Maybe String)]
    case result of
        [(email, name, _, _)] -> do
            execute conn "UPDATE users SET is_active = ?, block_reason = ? WHERE id =?" (0 :: Int, Just reason, clientId)
            putStrLn $ "Client with ID " ++ clientId ++ " was blocked."
            -> putStrLn "Client not found"