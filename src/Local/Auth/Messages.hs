{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Local.Auth.Messages where

import Import.NoFoundation


data LocalAuthMessage
    = SignInTitle
    deriving (Show)

defaultMessage :: LocalAuthMessage -> Text
defaultMessage = russianMessage

russianMessage :: LocalAuthMessage -> Text
russianMessage SignInTitle = "Вход"

instance RenderMessage master LocalAuthMessage where
    renderMessage _ ("ru":_) = russianMessage
    renderMessage _ _ = defaultMessage
