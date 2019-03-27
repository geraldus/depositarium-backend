{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
module Local.Auth.Plugin
    ( PrizmAuthPlugin
    , authPrizm
    , loginR ) where


import           Import.NoFoundation

import qualified Local.Auth.Messages           as PMsg

import           Yesod.Auth                    ( AuthHandler, AuthPlugin (..),
                                                 AuthRoute, Creds (..),
                                                 Route (..), YesodAuth,
                                                 loginErrorMessageI,
                                                 setCredsRedirect )
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
loginR = PluginR "PRIZM Yesod Auth Plugin" ["login"]

class ( YesodAuth site
      , YesodPersist site
      , BaseBackend (YesodPersistBackend site) ~ SqlBackend
      , PersistQueryRead (YesodPersistBackend site)
      , PersistUniqueRead (YesodPersistBackend site) )
      => PrizmAuthPlugin site

authPrizm :: PrizmAuthPlugin m => AuthPlugin m
authPrizm =
  AuthPlugin "PRIZM Yesod Auth Plugin" dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch _ _              = notFound
    loginWidget toMaster = do
      request <- getRequest
      [whamlet|
        $newline never
        <h1>_{PMsg.SignInTitle}
        <form method="post" action="@{toMaster loginR}">
          $maybe t <- reqToken request
            <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
          <table>
            <tr>
              <th>_{Msg.UserName}
              <td>
                 <input type="text" name="username" required>
            <tr>
              <th>_{Msg.Password}
              <td>
                 <input type="password" name="password" required>
            <tr>
              <td colspan="2">
                 <button type="submit" .btn .btn-success>_{Msg.LoginTitle}
        |]


postLoginR :: (PrizmAuthPlugin site)
           => AuthHandler site TypedContent
postLoginR = do
    (username, password) <- runInputPost
        ((,) Control.Applicative.<$> ireq textField "username"
             Control.Applicative.<*> ireq textField "password")
    authRes <- checkCreds username password
    -- TODO: FIXME: Check if user have verified email address
    if authRes == AuthSuccess
        then setCredsRedirect (Creds "PRIZM Yesod Auth Plugin" username [])
        else loginErrorMessageI LoginR
                (if authRes == InvalidAuthPair
                    then Msg.InvalidUsernamePass
                    else Msg.IdentifierNotFound username)


checkCreds :: (PrizmAuthPlugin site) => Text -> Text -> AuthHandler site AuthResult'
checkCreds username password = liftHandler . runDB $ do
    mayEmail <- getBy $ UniqueEmail username
    case mayEmail of
        Nothing   -> do
            userCount <- count ([] :: [Filter User])
            root <- selectFirst [ UserIdent ==. username ] [ ]
            if userCount > 1
                then pure NoSuchUser
                else pure $ case root of
                    Nothing -> NoSuchUser
                    Just user -> if isValidPass
                            password (userPassword . entityVal $ user)
                        then AuthSuccess
                        else InvalidAuthPair
        Just email -> do
            mayUser <- get . emailUser . entityVal $ email
            case mayUser of
                Nothing -> pure NoSuchUser
                Just user -> pure $
                    if isValidPass password (userPassword user)
                        then AuthSuccess
                        else InvalidAuthPair


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
    = AuthSuccess
    | NoSuchUser
    | InvalidAuthPair
    deriving Eq
