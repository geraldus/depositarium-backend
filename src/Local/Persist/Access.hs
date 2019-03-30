{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Local.Persist.Access where

import           ClassyPrelude.Yesod
import           Data.Aeson


data AccessType
    -- super user
    -- users
    = ListUsers -- ^ all site users
    | CreateUser
    | ViewUser -- ^ all user data
    | UpdateUser
    | DeleteUser
    -- currencies
    | ListCurrency
    | CreateCurrency
    | UpdateCurrency
    | DeleteCurrency
    -- treasurer
    | ListDeposit
    | ListWithdrawal
    | ConfirmDeposit
    | ConfirmWithdrawal
    | RejectDeposit
    | RejectWithdrawal
    -- operator
    | MakeDeposit
    | RequestWithdrawal
    | SelfBalanceView
    | SelfBalanceHistory
    | SelfListDeposit
    | SelfListWithdrawal
    | SelfDepositCancel
    | SelfWithdrawalCancel

    deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "AccessType"

instance ToJSON AccessType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON AccessType

data AccessRightJ = AccessRightJ
    { accessRightJName  :: Text
    , accessRightJTitle :: Text
    , accessRightJDesc  :: Text }
instance ToJSON AccessRightJ where
    toJSON AccessRightJ{..} = object
        [ "name"  .= toJSON accessRightJName
        , "title" .= toJSON accessRightJTitle
        , "desc"  .= toJSON accessRightJDesc ]
