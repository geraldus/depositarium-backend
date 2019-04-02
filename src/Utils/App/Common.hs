{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Utils.App.Common where

import           Import
import           Utils.Form ( processFormJ )

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
