{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
module Handler.Manage.User.Update where

import           Import                  hiding ( delete, on, update, (!=.),
                                           (=.), (==.) )

import           Form.UserData
import           Local.Persist.Access
import           Type.UserData
import           Utils.Common
import           Utils.Database.Password ( saltPass )
import           Utils.Database.UserData

import           Data.Aeson              as A
import           Database.Esqueleto

getManageUpdateUserR :: UserId -> Handler Html
getManageUpdateUserR user = defaultLayout $ do
    -- TODO: implement something like: react component -> config
    -- and handle this in react app
    (u, e, m, rs) <- liftHandler $ getUserData404 user
    let userDataJSON = preEscapedToMarkup . encodeStrictText . jsonMerge $
            [ cleanUpUser u
            , cleanUpEmail e
            , cleanUpMeta m
            , object [ "rights" .= cleanUpRights rs ] ]
    validateRoute <- getCurrentRoute
    let accessRightsJSON = encodeStrictText allAccessRightsJ
    [whamlet|manager-update-user
        <script>
            ((window) => { window.CURRENT_ROUTE_CONFIG = {
                $maybe route <- validateRoute
                    path: '@{route}',
                exact: false,
                componentName: "manageUpdateUser",
                props: {
                    apiUrl: '@?{(ManageUpdateUserR user, [("_accept", "application/json")])}',
                    accessRights: #{preEscapedToMarkup accessRightsJSON},
                    user: #{userDataJSON}
                }
            } })(window)
        |]

postManageUpdateUserR :: UserId -> Handler TypedContent
postManageUpdateUserR user = selectRep $ provideRep $ do
        msg <- getMessageRender
        (userData', userRights) <- getUserData user
        case userData' of
            (u, e, m):_ -> do
                formData <- runInputPostResult $ userDataIForm True
                processForm
                    msg
                    formData
                    (u, entityKey <$> e, entityKey <$> m, userRights)
            _ -> sendStatusJSON status404 . errorResponseJ $
                    msg MsgNotFoundErrorText
    where
        processForm msg FormMissing _ = pure $
            dataMissingResponse msg
        processForm msg (FormFailure errors) _ = pure $
            formErrorsResponse msg errors
        processForm _ (FormSuccess fd) ud = do
            let (u, e, m, _) = ud
            ud <- updateUser
                (entityKey u) e m fd
            return $ object
                [ "type"   .= A.String "success"
                , "status" .= A.String "success" ]

        dataMissingResponse msg = errorResponseJ (msg MsgWrongRequest)

        formErrorsResponse msg errors = errorResponseWithData
            (msg MsgFormFailureErrorText)
            [ "form-errors" .= map toJSON errors ]

        errorResponseWithData errorText =
            jsonMerge . (:) (errorResponseJ errorText) . (:[]) . object

getUserData404
    :: UserId
    -> Handler
        ( Entity User
        , Maybe (Entity Email)
        , Maybe (Entity UserMeta)
        , [ Entity UserRights ] )
getUserData404 user = do
    (mayData, rs) <- getUserData user
    case mayData of
        []          -> notFound
        (u, e, m):_ -> pure (u, e, m, rs)

getUserData
    :: UserId
    -> Handler
            ( [ ( Entity User
                , Maybe (Entity Email)
                , Maybe (Entity UserMeta) ) ]
            , [ Entity UserRights ] )
getUserData user = do
    userData <- runDB . select $
        from $ \(u `LeftOuterJoin` e `LeftOuterJoin` m) -> do
            on (just (u ^. UserId) ==. (m ?. UserMetaUser))
            on (just (u ^. UserId) ==. (e ?. EmailUser))
            where_ (u ^. UserId ==. val user)
            return (u, e, m)
    userRights <- runDB . select $ from $ \r -> do
        where_ (val user ==. r ^. UserRightsUser )
        return r
    return (userData, userRights)

updateUser
    :: UserId
    -> Maybe EmailId
    -> Maybe UserMetaId
    -> UserData
    -> Handler ()
updateUser user email meta UserData{..} = do
        saltedPass <- liftIO $ saltPass userDataPassword
        let emailData = mkVerifiedEmail userDataEmail
        let metaData = mkUserMeta
                userDataFirstName userDataPatronymic userDataLastName
        let acccessRights = map mkUserRights userDataRights
        runDB $ do
            update $ \u -> do
                let userIdentUpdate = UserIdent =. val userDataIdent
                    userPasswordUpdate =
                        [ UserPassword =. val saltedPass | userDataPassword /= "" ]
                set u (userIdentUpdate : userPasswordUpdate)
                where_ (u ^. UserId ==. val user)
            let emailUpdates = [ EmailEmail =. val userDataEmail ]
            let metaUpdates =
                    [ UserMetaFirstName  =. val userDataFirstName
                    , UserMetaPatronymic =. val userDataPatronymic
                    , UserMetaLastName   =. val userDataLastName ]
            case email of
                Just eid -> update $ \e ->
                    set e emailUpdates >> where_ (e ^. EmailId ==. val eid)
                Nothing     -> void $ insert emailData
            case meta of
                Just mid -> update $ \m ->
                    set m metaUpdates >> where_ (m ^. UserMetaId ==. val mid)
                Nothing    -> void $ insert metaData
            delete . from $ \rs ->
                where_ (rs ^. UserRightsUser ==. val user)
            insertMany_ acccessRights
            -- return (Entity uid user, Entity eid email, Entity mid meta, rids)
    where

        mkVerifiedEmail :: Text -> Email
        mkVerifiedEmail e = Email e user Nothing

        mkUserMeta :: Text -> Text -> Text -> UserMeta
        mkUserMeta = UserMeta user

        mkUserRights :: AccessType -> UserRights
        mkUserRights = UserRights user
