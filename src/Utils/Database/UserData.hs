{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Utils.Database.UserData where

import           Import.NoFoundation as I hiding ( on, (==.) )
import           Utils.Common        ( jsonMerge )

import           Data.Aeson          as A
import           Database.Esqueleto


-- getUserMetaData :: UserId -> (Entity User, Maybe (Entity Email), Maybe (Entity UserMeta), [AccessType])
getUserMetaData ::
    forall (m :: * -> *) backend.
    (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend)
    => Key User
    -> ReaderT backend m
        (Maybe
            ( Entity User
            , Maybe (Entity Email)
            , Maybe (Entity UserMeta)
            , [Entity UserRights] ))
getUserMetaData user = do
    meta <- select . from $ \(u `LeftOuterJoin` e `LeftOuterJoin` m) -> do
        on (just (u ^. UserId) ==. m ?. UserMetaUser)
        on (just (u ^. UserId) ==. e ?. EmailUser)
        where_ (u ^. UserId ==. val user)
        return (u, e, m)
    withExistingUser meta $ \(u, e, m) -> do
        access <- select . from $ \a -> do
            where_ (a ^. UserRightsUser ==. val user)
            return a
        return (u, e, m, access)
    where
        withExistingUser [] _         = pure Nothing
        withExistingUser (m:_) action = Just <$> action m


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
