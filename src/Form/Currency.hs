{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Form.Currency where

import           Import
import           Local.Persist.Currency

import           Utils.Common           ( orEmpty )


currencyIForm :: FormInput Handler Currency
currencyIForm = Currency
        <$> ireq textField "ident"
        <*> ireq textField "code"
        <*> ireq textField "sign"
        <*> (cryptoTypeFromBool <$> ireq boolField "isCrypto")
    where
        cryptoTypeFromBool True  = CryptoCurrency
        cryptoTypeFromBool False = FiatCurrency

currencyDescIForm
    :: CurrencyId -> FormInput Handler CurrencyDesc
currencyDescIForm idt = CurrencyDesc
    <$> pure idt
    <*> ireq textField "language"
    <*> (orEmpty <$> iopt textField "abbr")
    <*> (orEmpty <$> iopt textField "htmlName")
    <*> (orEmpty <$> iopt textField "htmlLongName")
    <*> (orEmpty <$> iopt textField "htmlDesc")
