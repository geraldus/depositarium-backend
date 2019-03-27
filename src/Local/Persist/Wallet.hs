{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Local.Persist.Wallet where

import           ClassyPrelude.Yesod
import           Data.Aeson


data TransactionType
    -- | Int's are money amount in cents.  All income operations
    --   must have positive sign; outcome -- negative.
    = TTDeposit             -- ^ + positive
    | TTWithdrawal          -- ^ - negative
    | TTWithdrawalCancel    -- ^ + positive
    | TTWithdrawalReject    -- ^ + positive
    | TTWithdrawalArchieve  -- ^ + positive
    | TTParamining          -- ^ + positive
    | TTPenality Int        -- ^ - negative
    | TTBonus Int           -- ^ + positive
    deriving (Generic, Show, Read, Eq)
derivePersistField "TransactionType"

instance ToJSON TransactionType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON TransactionType
