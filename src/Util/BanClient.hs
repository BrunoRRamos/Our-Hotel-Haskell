module BanClient
    ( blockClient
    ) where

import Models.User (User(..))

blockClient :: User -> Maybe String -> User
blockClient user (Just reason) = user { _isActive = False, _blockReason = Just reason}
blockClient user Nothing = user { _isActive = False, _blockReason = Just "Razão não especificada"}