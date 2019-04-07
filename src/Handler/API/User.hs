{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.API.User where

import           Import

import           Local.Persist.Access    ( AccessType (..) )
import           Utils.Common            ( errorResponseJ, jsonMerge )
import           Utils.Database.UserData ( cleanJSONUserData, cleanUpUser,
                                           getUserMetaData )

import           Network.HTTP.Types      ( status200, unauthorized401 )


-- | Return shallow authenticated user information
postAPIAuthInfoR :: Handler TypedContent
postAPIAuthInfoR = do
    msg <- getMessageRender >>= \x -> pure $ x MsgNotAuthenticated
    auth <- maybeAuth
    cleaned <- case auth of
        Nothing -> pure $ guestUser msg
        Just (Entity u _) -> do
            meta <- runDB $ getUserMetaData u
            case meta of
                Nothing -> pure $ guestUser msg
                Just (u, e, m, rs) ->  pure $ jsonMerge
                    [ cleanJSONUserData u e m rs
                    , object [ "auth" .= toJSON True ] ]
    sendJSON cleaned
    where
        guestUser msg = object
            [ "message" .= msg
            , "auth" .= toJSON False
            , "rights" .= array ([] :: [Text]) ]

-- | Return user metadata.  Intended to be used without
-- authorization check.  Returns data when:
-- * either requested data belongs to logged in user
-- * or logged in user have 'UserRead' access rights set
postAPIUserMetaDataUnsafeR :: UserId -> Handler TypedContent
postAPIUserMetaDataUnsafeR user = do
    (s, r) <- maybeAuth >>= withLoggedUser (\l ->
        withOwnerOrPriviledged l (\ _ ->
                runDB (getUserMetaData user)
                    >>= withExistingMeta (\(u, e, m, rs) -> pure
                            (status200, cleanJSONUserData u e m rs))))
    sendStatusJSON s r
  where
        withLoggedUser action (Just l) = action l
        withLoggedUser _ Nothing       = return401

        withOwnerOrPriviledged (Entity logged _) action =
            runDB (getUserMetaData logged)
                >>= withExistingMeta (\ meta @ (u @ (Entity i _), e, m, rs) ->
                    if i == user
                        then pure ( status200 , cleanJSONUserData u e m rs)
                        else withPrivileged meta action)

        withPrivileged p@(_, _, _, rs) action
            | ViewUser `elem` map userRightsAccess (vals rs) = action p
            | otherwise = return401

        withExistingMeta _ Nothing       = return404
        withExistingMeta action (Just m) = action m

        return401 = renderErrorResponse unauthorized401 MsgAccessDenied
        return404 = renderErrorResponse unauthorized401 MsgAccessDenied

        renderErrorResponse s msg = do
            render <- getMessageRender
            pure ( s, errorResponseJ (render msg) )

        vals = map entityVal

sendJSON = sendStatusJSON status200
