{-# LANGUAGE DeriveGeneric #-}
module Local.Persist.Summary where

import           ClassyPrelude.Yesod
import           Data.Aeson


data SummaryType
    = PZMBankTotal
    deriving (Generic, Show, Read, Eq)
derivePersistField "SummaryType"

instance ToJSON SummaryType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SummaryType
