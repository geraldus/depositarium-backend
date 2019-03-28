{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Manage.User.Create where

import           Import

import           Local.Persist.Access
import           Utils.Database.Password ( saltPass )

import           Data.Aeson              as A
import qualified Data.HashMap.Lazy       as HML


getManageCreateUserR :: Handler Html
getManageCreateUserR = defaultLayout [whamlet|user-create|]

postManageCreateUserR :: Handler TypedContent
postManageCreateUserR = do
    msg <- getMessageRender
    ((formData, _), _) <- runFormPost userForm
    res <- processForm msg formData
    selectRep . provideRep $ pure res
    where
        processForm msg FormMissing = pure $
            dataMissingResponse msg
        processForm msg (FormFailure errors) = pure $
            formErrorsResponse msg errors
        processForm _ (FormSuccess fd) = do
            ud <- createUser fd
            return $ object [ "type" .= A.String "success" ]

        dataMissingResponse msg = errorResponse (msg MsgWrongRequest)

        formErrorsResponse msg errors = errorResponseWithData
            (msg MsgFormFailureErrorText)
            [ "form-errors" .= map toJSON errors ]

        errorResponseWithData errorText =
            merge . (:) (errorResponse errorText) . (:[]) . object

        errorResponse errorText = object
            [ "type" .= A.String "error"
            , "message" .= toJSON errorText ]

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


data UserData = UserData
    { userDataFirstName  :: Text
    , userDataPatronymic :: Text
    , userDataLastName   :: Text
    , userDataIdent      :: Text
    , userDataEmail      :: Text
    , userDataPassword   :: Text
    , userDataRights     :: [AccessType] }

userForm :: Form UserData
userForm _extra = do
    (name, _) <- mreq textField "name" Nothing
    (patronymic, _) <- mreq textField "patronymic" Nothing
    (lastname, _) <- mreq textField "lastname" Nothing
    (ident, _) <- mreq textField "ident" Nothing
    (email, _) <- mreq textField "email" Nothing
    (password, _) <- mreq textField "password" Nothing
    (accessRights, _) <- mreq
            (checkboxesField (pure accessRightsOpts))
            "rights"
            Nothing
    let res = UserData
            <$> name
            <*> patronymic
            <*> lastname
            <*> ident
            <*> email
            <*> password
            <*> accessRights
    return (res, [whamlet||])
    where
        accessRightsOpts = mkOptionList accessRightsList

        accessRightsList :: [ Option AccessType ]
        accessRightsList = map
            (\r -> Option (txt r) r (txt r))
            [minBound .. maxBound]

        txt = pack . show

merge :: [Value] -> Value
merge = Object . HML.unions . map (\(Object x) -> x)
