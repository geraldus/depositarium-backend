{-# LANGUAGE OverloadedStrings #-}
module Handler.Manage.User.List where

import           Import             hiding ( on, (==.) )

import           Database.Esqueleto


getManageListUsersR :: Handler TypedContent
getManageListUsersR = do
    list <- runDB . select . from $
        \(e `LeftOuterJoin` u `LeftOuterJoin` m) -> do
            on (m ^. UserMetaUser ==. u ^. UserId)
            on (e ^. EmailUser ==. u ^. UserId)
            return (u, e, m)
    selectRep . provideRep . return $ object
        [ "users" .= map toJSON list ]
