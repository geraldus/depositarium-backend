{-# LANGUAGE OverloadedStrings #-}
module Handler.Manage.User.List where

import           Import             hiding ( on, (!=.), (==.) )

import           Data.Aeson         as A
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
        clean (u, _, _) = cleanUpUser u
        cleanUpUser :: Entity User -> A.Value
        cleanUpUser (Entity idx v) = object
            [ "id" .= toJSON idx
            , "ident" .= toJSON (userIdent v) ]
