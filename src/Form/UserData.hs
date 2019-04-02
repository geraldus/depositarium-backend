{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Form.UserData where

import           Import
import           Local.Persist.Access

import           Type.UserData
import           Utils.Common         ( orEmpty )

import           Data.Aeson           ( decode )
import           Text.Read            ( readMaybe )


userDataIForm :: Bool -> FormInput Handler UserData
userDataIForm passwordless = UserData
    <$> ireq textField "firstName"
    <*> (orEmpty <$> iopt textField "patronymic")
    <*> ireq textField "lastName"
    <*> ireq textField "ident"
    <*> ireq textField "email"
    <*> (if passwordless then orEmpty <$> pass iopt else pass ireq)
    <*> (jsonArray <$> ireq textField "rights")
    where
        pass o = o textField "password"
        jsonArray :: Text -> [ AccessType ]
        jsonArray =
            catMaybes . mread . decode . encodeUtf8 . fromStrict

        mread = maybe [] (map readMaybe)
