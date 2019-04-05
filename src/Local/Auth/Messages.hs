{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Local.Auth.Messages where

import           Import.NoFoundation


data LocalAuthMessage
    = SignInTitle
    | ErrorJSONOnlyAPI
    | SignInSuccess
    deriving (Show)

defaultMessage :: LocalAuthMessage -> Text
defaultMessage = russianMessage

russianMessage :: LocalAuthMessage -> Text
russianMessage SignInTitle      = "Вход"
russianMessage ErrorJSONOnlyAPI = "Этот способ входа работает только через JSON API"
russianMessage SignInSuccess    = "Вход выполнен"


instance RenderMessage master LocalAuthMessage where
    renderMessage _ ("ru":_) = russianMessage
    renderMessage _ _        = defaultMessage
