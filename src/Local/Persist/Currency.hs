{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Local.Persist.Currency where

import           ClassyPrelude.Yesod
import           Data.Aeson


data CurrencyType
    = FiatCurrency
    | CryptoCurrency
    deriving (Generic, Show, Read, Eq)
derivePersistField "CurrencyType"

instance ToJSON CurrencyType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CurrencyType