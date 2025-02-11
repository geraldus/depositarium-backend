{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Utils.Database.UserData where

import           Import.NoFoundation as I hiding ( on, (==.), (||.) )
import           Utils.Common        ( jsonMerge )

import           Data.Aeson          as A
import           Database.Esqueleto


type MaybeEntity a = Maybe (Entity a)

type PartialMetas = (Entity User, MaybeEntity Email, MaybeEntity UserMeta)

type UserMetas = (Entity User, MaybeEntity Email, MaybeEntity UserMeta, [Entity UserRights])

newtype SafeUserMetasJSON = SafeUserMetas { cleanMetasJSON :: UserMetas -> A.Value }

zipPartialMetas :: PartialMetas -> [Entity UserRights] -> UserMetas
zipPartialMetas (u, e, m) = (,,,) u e m

unCurryMetas :: (u -> e -> m -> rs -> x) -> (u, e, m, rs) -> x
unCurryMetas f (u, e, m, rs) = f u e m rs


getUserMetaData ::
    ( MonadIO m
    , BackendCompatible SqlBackend backend
    , PersistQueryRead backend
    , PersistUniqueRead backend)
    => Key User -> ReaderT backend m (Maybe UserMetas)
getUserMetaData = getUserMetaDataEither . Left

getUserMetaDataEither ::
    ( MonadIO m
    , BackendCompatible SqlBackend backend
    , PersistQueryRead backend
    , PersistUniqueRead backend)
    => Either (Key User) Text -> ReaderT backend m (Maybe UserMetas)
getUserMetaDataEither user =
    mayExists <$> selectFilterUserMetas ( whereCond user )
    where
        whereCond (Left k)  = \u _ _ -> byIdKeyCond k u
        whereCond (Right t) = \u e _ -> byTextIdentCond t u e

        byIdKeyCond idKey candidate =
            candidate ^. UserId ==. val idKey

        byTextIdentCond ident userC emailC =
                userC ^. UserIdent ==. val ident
            ||. emailC?. EmailEmail ==. just (val ident)

        mayExists = headMay

selectFilterUserMetas ::
    ( MonadIO m
    , BackendCompatible SqlBackend backend
    , PersistQueryRead backend
    , PersistUniqueRead backend)
    => ( SqlExpr (Entity User)
            -> SqlExpr (MaybeEntity Email)
            -> SqlExpr (MaybeEntity UserMeta)
            -> SqlExpr (Database.Esqueleto.Value Bool)
            )
    -> ReaderT backend m [ UserMetas ]
selectFilterUserMetas applyPartialWhere = do
    meta <- selectPartialMetas applyPartialWhere
    forM meta $ \(u, e, m) -> do
        access <- select . from $ \a -> do
            where_ (a ^. UserRightsUser ==. val (entityKey u))
            return a
        return (u, e, m, access)

selectPartialMetas ::
    ( MonadIO m
    , BackendCompatible SqlBackend backend
    , PersistQueryRead backend
    , PersistUniqueRead backend)
    => ( SqlExpr (Entity User)
            -> SqlExpr (MaybeEntity Email)
            -> SqlExpr (MaybeEntity UserMeta)
            -> SqlExpr (Database.Esqueleto.Value Bool)
            )
    -> ReaderT backend m [PartialMetas]
selectPartialMetas applyWhere = select . from $
    \(u `LeftOuterJoin` e `LeftOuterJoin` m) -> do
        on (just (u ^. UserId) ==. m ?. UserMetaUser)
        on (just (u ^. UserId) ==. e ?. EmailUser)
        where_ (applyWhere u e m)
        return (u, e, m)


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
