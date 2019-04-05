{-# LANGUAGE OverloadedStrings #-}
module Utils.Database.UserData where

import           Import.NoFoundation as I
import           Utils.Common        ( jsonMerge )

import           Data.Aeson          as A




cleanJSONUserData ::
       Entity User
    -> Maybe (Entity Email)
    -> Maybe (Entity UserMeta)
    -> [Entity UserRights]
    -> A.Value
cleanJSONUserData u e m rs = jsonMerge
        [ cleanUpUser u
        , cleanUpEmail e
        , cleanUpMeta m
        , object [ "accessRights" .= cleanUpRights rs ] ]

cleanUpUser :: Entity User -> A.Value
cleanUpUser (Entity idx v) = object
    [ "id"    .= toJSON idx
    , "ident" .= toJSON (userIdent v) ]

cleanUpEmail :: Maybe (Entity Email) -> A.Value
cleanUpEmail Nothing = object []
cleanUpEmail (Just (Entity _ v)) = object
    [ "email" .= toJSON (emailEmail v)
    , "verified" .= toJSON (I.isNothing . emailVerkey $ v) ]

cleanUpMeta :: Maybe (Entity UserMeta) -> A.Value
cleanUpMeta Nothing = object []
cleanUpMeta (Just (Entity _ v)) = object
    [ "firstName" .= toJSON (userMetaFirstName v)
    , "patronymic" .= toJSON (userMetaPatronymic v)
    , "lastName" .= toJSON (userMetaLastName v) ]

cleanUpRights :: [ Entity UserRights ] -> A.Value
cleanUpRights = toJSON . map (userRightsAccess . entityVal)
