{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Local.Persist.Access where

import           ClassyPrelude.Yesod
import           Data.Aeson


data AccessType
    = ListUsers
    | CreateUser
    | ViewUser
    | UpdateUser
    | DeleteUser
    | SelfBalanceView
    deriving (Generic, Show, Read, Eq, Enum, Bounded)
derivePersistField "AccessType"

instance ToJSON AccessType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON AccessType
