{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
module Schema (module Schema) where

import Database.Beam
    ( Generic, Beamable, Table(..), Columnar, Identity )

import Data.Text (Text)

data UserT f
    = User
    { _userEmail     :: Columnar f Text
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving (Generic, Beamable)
          
type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UserId . _userEmail