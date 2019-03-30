{-# LANGUAGE OverloadedStrings #-}
module Handler.Manage.User.List where

import           Import                  as I hiding ( on, (!=.), (==.) )
import           Utils.Common            ( jsonMerge )
import           Utils.Database.UserData

import           Database.Esqueleto


getManageListUserR :: Handler TypedContent
getManageListUserR = do
    list <- runDB . select . from $
        \(u `LeftOuterJoin` e `LeftOuterJoin` m) -> do
            on (just (u ^. UserId) ==. m ?. UserMetaUser)
            on (just (u ^. UserId) ==. e ?. EmailUser)
            where_ (u ^. UserIdent !=. val "")
            return (u, e, m)
    selectRep $ do
        provideRep . defaultLayout $ [whamlet|user-list|]
        provideRep . return $ object
            [ "users" .= map (toJSON . clean) list ]
    where
        clean (u, e, m) = jsonMerge [ cleanUpUser u, cleanUpEmail e, cleanUpMeta m ]
