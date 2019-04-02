{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Manage.User.Create where

import           Import

import           Form.UserData
import           Local.Persist.Access
import           Type.UserData
import           Utils.App.Common        ( processAppFormJ )
import           Utils.Common
import           Utils.Database.Password ( saltPass )

import           Data.Aeson              as A


getManageCreateUserR :: Handler Html
getManageCreateUserR = defaultLayout [whamlet|user-create|]

-- FIXME: Generalize over Message Render, e.g. MessageRender -> Handler ...
postManageCreateUserR :: Handler TypedContent
postManageCreateUserR = do
    formData <- runInputPostResult $ userDataIForm False
    res <- processForm formData
    selectRep . provideRep $ pure res
    where
        processForm result = processAppFormJ
            result
            $ \fd ->  do
                ud <- createUser fd
                return $ object [ "type" .= A.String "success" ]


accessRightsList :: [ (Text, AccessType) ]
accessRightsList = map (\r -> (txt r, r)) [minBound .. maxBound]

createUser :: UserData -> Handler (Entity User, Entity Email, Entity UserMeta, [ UserRightsId ])
createUser UserData{..} = do
    user <- liftIO $ withSaltedPassword userDataIdent userDataPassword
    runDB $ do
        uid <- insert user
        let email = mkVerifiedEmail uid userDataEmail
        let meta = mkUserMeta
                uid userDataFirstName userDataPatronymic userDataLastName
        let acccessRights = map (mkUserRights uid) userDataRights
        eid <- insert email
        mid <- insert meta
        rids <- insertMany acccessRights
        return (Entity uid user, Entity eid email, Entity mid meta, rids)
    where
        withSaltedPassword :: Text -> Text -> IO User
        withSaltedPassword ident = (User ident <$>) . saltPass

        mkVerifiedEmail :: UserId -> Text -> Email
        mkVerifiedEmail uid email = Email email uid Nothing

        mkUserMeta :: UserId -> Text -> Text -> Text -> UserMeta
        mkUserMeta = UserMeta

        mkUserRights :: UserId -> AccessType -> UserRights
        mkUserRights = UserRights
