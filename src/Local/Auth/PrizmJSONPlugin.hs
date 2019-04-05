{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
module Local.Auth.PrizmJSONPlugin
    ( PrizmJSONAuthPlugin
    , prizmJSONAuth
    , loginR ) where


import           Import.NoFoundation


import qualified Local.Auth.Messages           as PMsg
import           Utils.Common                  ( errorResponseJ,
                                                 successResponseWithDataJ )
import           Utils.Database.UserData       ( cleanUpUser )

import           Yesod.Auth                    ( AuthHandler, AuthPlugin (..),
                                                 AuthRoute, Route (..),
                                                 YesodAuth )
import qualified Yesod.Auth.Message            as Msg
import           Yesod.Form                    ( ireq, runInputPost, textField )

import           Control.Applicative           ( (<$>), (<*>) )
import qualified Crypto.Hash                   as H
import           Data.ByteArray                ( convert )
import           Data.ByteString.Base16        as B16
import           Data.Text                     ( Text )
import qualified Data.Text.Encoding            as TE
import qualified Yesod.Auth.Util.PasswordStore as PS



loginR :: AuthRoute
loginR = PluginR "Prizm.  JSON based auth plugin for Yesod Framework" ["login"]

class ( YesodAuth site
      , YesodPersist site
      , BaseBackend (YesodPersistBackend site) ~ SqlBackend
      , PersistQueryRead (YesodPersistBackend site)
      , PersistUniqueRead (YesodPersistBackend site) )
      => PrizmJSONAuthPlugin site

prizmJSONAuth :: PrizmJSONAuthPlugin m => AuthPlugin m
prizmJSONAuth =
  AuthPlugin "prizm-json-plugin" dispatch loginWidget
    where
        dispatch "POST" ["login"] = postLoginR >>= sendResponse
        dispatch _ _              = notFound

        loginWidget _ = getMessageRender >>= \r ->
                [whamlet|#{r PMsg.ErrorJSONOnlyAPI}|]


postLoginR :: (PrizmJSONAuthPlugin site)
           => AuthHandler site TypedContent
postLoginR = do
    (username, password) <- runInputPost basicForm
    authRes <- checkCreds username password
    render <- getMessageRender
    (status, response) <- case authRes of
        -- In our case no need to run 'authenticate', e.g.
        -- @authenticate (Creds "prizm-json" (userIdent . entityVal $ u)  [])@
        -- because we have an AuthId (all required DB data) at hands already.
        AuthSuccess u ->
            (,) status200 <$> setCredsResponse u
        InvalidAuthPair -> pure
            ( unauthorized401
            , errorResponseJ $ render Msg.InvalidUsernamePass )
        NoSuchUser -> pure
            ( unauthorized401
            , errorResponseJ . render $ Msg.IdentifierNotFound username )
    sendStatusJSON status response
    where
        basicForm = (,)
            <$> ireq textField "username"
            <*> ireq textField "password"

checkCreds ::
    ( PrizmJSONAuthPlugin site)
    => Text -> Text -> AuthHandler site AuthResult'
checkCreds username password = liftHandler . runDB $ do
    mayEmail <- getBy $ UniqueEmail username
    case mayEmail of
        Nothing   -> checkRootAuth username password
        Just email ->
            let userId = emailUser . entityVal $ email
            in  checkUserAuth password . fmap (Entity userId) <$> get userId

setCredsResponse  ::
    ( MonadHandler m, YesodAuth (HandlerSite m) ) --, RenderMessage (HandlerSite m) msg )
    => Entity User
    -> m Value
setCredsResponse user = do
    setSession "_ID" $ toPathPiece (entityKey user)
    pure . successResponseWithDataJ . toJSON $ cleanUpUser user

userAuthResult :: Entity User -> Text -> AuthResult'
userAuthResult u p
    | isValidPass p (userPassword . entityVal $ u) = AuthSuccess u
    | otherwise = InvalidAuthPair

checkUserAuth :: Text -> Maybe (Entity User) -> AuthResult'
checkUserAuth _ Nothing  = NoSuchUser
checkUserAuth p (Just u) = userAuthResult u p

checkRootAuth ::
    ( MonadIO m , PersistQueryRead backend, BaseBackend backend ~ SqlBackend )
    => Text -> Text -> ReaderT backend m AuthResult'
checkRootAuth u p
    | u == "root" = do
        cnt <- count ([] :: [Filter User])
        if cnt == 1
            then checkUserAuth p
                    <$> selectFirst [ UserIdent ==. u ] [ ]
            else pure InvalidAuthPair
    | otherwise = pure InvalidAuthPair


saltPass' :: String -> String -> String
saltPass' salt pass = salt ++ unpack (TE.decodeUtf8 $ B16.encode $ convert
        (H.hash (TE.encodeUtf8 $ pack $ salt ++ pass) :: H.Digest H.MD5))

isValidPass :: Text -- ^ cleartext password
        -> SaltedPass -- ^ salted password
        -> Bool
isValidPass ct salted =
    PS.verifyPassword (encodeUtf8 ct) (encodeUtf8 salted) || isValidPass' ct salted

isValidPass' :: Text -- ^ cleartext password
        -> SaltedPass -- ^ salted password
        -> Bool
isValidPass' clear' salted' =
    let salt = take saltLength salted
    in salted == saltPass' salt clear
  where
    clear = unpack clear'
    salted = unpack salted'
    saltLength = 5

type SaltedPass = Text

data AuthResult'
    = AuthSuccess (Entity User)
    | NoSuchUser
    | InvalidAuthPair
    deriving Eq
