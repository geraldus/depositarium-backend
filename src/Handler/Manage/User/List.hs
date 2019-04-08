{-# LANGUAGE OverloadedStrings #-}
module Handler.Manage.User.List where

import           Import                  as I hiding ( on, (!=.), (==.) )
import           Utils.Common            ( jsonMerge )
import           Utils.Database.UserData

import           Database.Esqueleto


getManageListUserR :: Handler TypedContent
getManageListUserR = do
    list <- runDB selectPatialAll
    selectRep . provideRep . return $ object
        [ "users" .= map (toJSON . clean) list ]
    where
        clean (u, e, m) = jsonMerge
            [ cleanUpUser u, cleanUpEmail e, cleanUpMeta m ]

selectPatialAll :: ReaderT SqlBackend Handler [PartialMetas]
selectPatialAll = selectPartialMetas (\_ _ _ -> val True)
