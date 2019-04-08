{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Utils.App.Common where

import           Import
import           Local.Persist.Access    ( AccessType (..) )
import           Utils.Common            ( encodeStrictText, errorResponseJ,
                                           send404JSON, sendJSON,
                                           successResponseWithDataJ )
import           Utils.Database.UserData ( UserMetas, cleanJSONUserData,
                                           getUserMetaData )
import           Utils.Form              ( processFormJ )

-- * Yesod App

getRenders :: WidgetFor App (Route App -> Text, AppMessage -> Text)
getRenders = (,) <$> liftHandler getUrlRender <*> liftHandler getMessageRender


setCompositeTitle
    :: (MonadWidget m, HandlerFor site ~ m, RenderMessage site msg)
    => [ msg ] -> m ()
setCompositeTitle ms = do
    r <- getMessageRender
    setTitle . toHtml . intercalate " | " . map r $ ms


-- * App Forms

processAppFormJ :: FormResult a -> (a -> Handler Value) -> Handler Value
processAppFormJ a onSuccess = do
    m <- getMessageRender
    processFormJ (m MsgWrongRequest, m MsgFormFailureErrorText) a onSuccess


-- * App Access

-- | Short circuiting action passing further only user with
-- spcific 'AccessType'
requireAccessJSON :: AccessType -> Handler UserMetas
requireAccessJSON r = do
    auth <- maybeAuth
    loggedUserOnly auth $ \user -> do
        meta <- runDB $ getUserMetaData (entityKey user)
        withAccess meta
    where
        loggedUserOnly Nothing _          = shortCircuit404
        loggedUserOnly (Just user) action = action user

        withAccess Nothing = shortCircuit404
        withAccess (Just metas @ (_, _, _, rs))
            | r `elem` map (userRightsAccess . entityVal) rs =
                pure $ metas
            | otherwise = shortCircuit404

        shortCircuit404 = do
            render <- getMessageRender
            send404JSON $ errorResponseJ (render MsgNotFound)
            -- send404JSON .
            --     ((flip errorResponseJ) MsgNotFound)


instance ToContent UserMetas where
    toContent = toContent . show
instance ToTypedContent UserMetas where
    toTypedContent = toTypedContent . toJSON
