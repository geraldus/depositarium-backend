{-# LANGUAGE DeriveGeneric #-}
module Local.Persist.Request where

import           ClassyPrelude.Yesod
import           Data.Aeson


data DepositStatusType
    = DSNew
    | DSClientConfirmed
    | DSClientCancelled
    | DSStaffRejected
    | DSStaffAccepted
    | DSStaffArchived
    | DSTimeout
    | DSExecuted
    deriving (Generic, Show, Read, Eq)
derivePersistField "DepositStatusType"

instance ToJSON DepositStatusType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON DepositStatusType


data WithdrawalStatusType
    = WSNew
    | WSClientCancelled
    | WSStaffRejected
    | WSStaffExecuted
    | WSStaffArchived
    | WSTimeoutArchieved
    deriving (Generic, Show, Read, Eq)
derivePersistField "WithdrawalStatusType"

instance ToJSON WithdrawalStatusType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON WithdrawalStatusType
