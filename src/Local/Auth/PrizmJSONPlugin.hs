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
import           Utils.Database.UserData       ( UserMetas, cleanJSONUserData,
                                                 getUserMetaDataEither )

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
      , BackendCompatible SqlBackend (YesodPersistBackend site)
      , BaseBackend (YesodPersistBackend site) ~ SqlBackend
      , PersistQueryRead (YesodPersistBackend site)
      , PersistUniqueRead (YesodPersistBackend site) )
      => PrizmJSONAuthPlugin site

prizmJSONAuth :: PrizmJSONAuthPlugin m => AuthPlugin m
prizmJSONAuth =
  AuthPlugin "prizm-json-plugin" dispatch loginWidget
    where
        dispatch "POST" ["login"]  = postLoginR
        dispatch "POST" ["logout"] = postLogoutR
        dispatch _ _               = notFound

        loginWidget _ = getMessageRender >>= \r ->
                [whamlet|#{r PMsg.ErrorJSONOnlyAPI}|]

postLogoutR :: (PrizmJSONAuthPlugin site)=> AuthHandler site TypedContent
postLogoutR = do
    deleteSession "_ID"
    render <- liftHandler getMessageRender
    sendStatusJSON status200 $ successResponseWithDataJ $ object
        [ "auth" .= toJSON False
        , "message" .= render PMsg.SignOutSuccess ]

postLoginR :: (PrizmJSONAuthPlugin site) => AuthHandler site TypedContent
postLoginR = do
    (username, password) <- runInputPost basicForm
    authRes <- checkCreds username password
    render <- getMessageRender
    (status, response) <- case authRes of
        -- In our case no need to run 'authenticate', e.g.
        -- @authenticate (Creds "prizm-json" (userIdent . entityVal $ u)  [])@
        -- because we have an AuthId (all required DB data) at hands already.
        AuthSuccess metas ->
            (,) status200 <$> setCredsResponse metas
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
    ( PrizmJSONAuthPlugin site )
    => Text -> Text -> AuthHandler site (AuthResultJSON UserMetas)
checkCreds username password = liftHandler . runDB $ do
    metas <- getUserMetaDataEither (Right username)
    pure $ case metas of
        Nothing -> NoSuchUser -- checkRootAuth username password
        Just m  -> userAuthResult m password

userAuthResult :: UserMetas -> Text -> AuthResultJSON UserMetas
userAuthResult metas @ (Entity _ u, _, _, _) p
    | isValidPass p (userPassword u) = AuthSuccess metas
    | otherwise = InvalidAuthPair

setCredsResponse  ::
    ( MonadHandler m, YesodAuth (HandlerSite m) )
    => UserMetas
    -> m Value
setCredsResponse metas = do
    let (user, e, m, rs) = metas
    setSession "_ID" $ toPathPiece (entityKey user)
    pure . successResponseWithDataJ $ cleanJSONUserData user e m rs

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

data ToJSON a => AuthResultJSON a
    = AuthSuccess a
    | NoSuchUser
    | InvalidAuthPair
    deriving Eq
