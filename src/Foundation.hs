{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Foundation where

import           Import.NoFoundation     as I hiding ( on, (==.), (||.) )
import           Yesod.Auth.Message
import           Yesod.Core.Types        ( Logger )
import qualified Yesod.Core.Unsafe       as Unsafe
import           Yesod.Default.Util      ( addStaticContentExternal )
import           Yesod.Form.I18n.Russian

import           Control.Monad.Logger    ( LogSource )
import qualified Data.CaseInsensitive    as CI
import qualified Data.Text.Encoding      as TE
import           Database.Persist.Sql    ( ConnectionPool, runSqlPool )
import           Text.Hamlet             ( hamletFile )
import           Text.Jasmine            ( minifym )

import           Local.Auth
import           Local.Persist.Access
import           Utils.Common
import           Utils.Database.UserData ( cleanUpUser )

import qualified Crypto.Nonce            as Nonce
import           Data.List               ( isSubsequenceOf )
import           Database.Esqueleto
import qualified Database.Persist        as P ( (==.) )
import           Text.Julius             ( RawJS (..) )

-- data AppChannels = AppChannels
--     { depositUserConfirm :: TChan (Entity DepositRequest)
--     , withdrawalRequest  :: TChan (Entity WithdrawalRequest) }

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appNonceGen    :: Nonce.Generator
    -- , appChannels    :: AppChannels
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

mkMessage "App" "messages" "ru"

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        fromMaybe
            (getApprootText guessApproot app req) (appRoot $ appSettings app)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        user <- maybeAuthPair
        let userJSON = maybe
                (object [ "auth" .= toJSON False ])
                (jsonMerge
                    . (:) (object [ "auth" .= toJSON True])
                    . (: []) . (toJSON . cleanUpUser . uncurry Entity))
                user
        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        let accessRightsJSON = encodeStrictText allAccessRightsJ
        pc <- widgetToPageContent $ do
            $(widgetFile "default-layout")
            addUiBundle
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _             = return Authorized
    isAuthorized HomeR _                 = return Authorized
    isAuthorized ManageListUserR _       = authorizeByAccess [ ListUsers, UpdateUser ]
    isAuthorized ManageCreateUserR _     = authorizeByAccess [ CreateUser ]
    isAuthorized (ManageUpdateUserR _) _ = authorizeByAccess [ UpdateUser ]
    isAuthorized ManageListCurrencyR _ = authorizeByAccess [ ListCurrency ]
    isAuthorized ManageCreateCurrencyTemporalR _ =
            authorizeByAccess [ CreateCurrency ]
    isAuthorized FaviconR _              = return Authorized
    isAuthorized RobotsR _               = return Authorized
    isAuthorized (StaticR _) _           = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger


-- | Authorizes only users having ALL specified access rights
authorizeByAccess :: [ AccessType ] -> Handler AuthResult
authorizeByAccess ats = do
    user <- maybeAuth
    case user of
        Nothing -> unauthorizedI MsgPleaseLogInText
        Just (Entity ident _) -> do
            accessRights <- map (userRightsAccess . entityVal) <$> getUserRights ident
            if isSubsequenceOf (sort ats) (sort accessRights)
                then pure Authorized
                else unauthorizedI MsgAccessDenied
    where
            getUserRights user = runDB $
                selectList [ UserRightsUser P.==. user ] []


-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb r = do
        mr <- getMessageRender
        -- TODO: FIXME: Do not show breadcrumbs for non-staff users
        breadcrumb' mr r
      where
        breadcrumb'
            :: (AppMessage -> Text)
            -> Route App  -- ^ The route the user is visiting currently.
            -> Handler (Text, Maybe (Route App))
        breadcrumb' _ _          = return ("*", Nothing)


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance PrizmJSONAuthPlugin App

instance YesodAuth App where
    type AuthId App = UserId
    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = False

    -- According to Yesod.Auth documentation for 'setCreds':
    -- @@@
    -- Sets user credentials for the session after checking them
    -- with authentication backends.
    -- @@@
    -- Plugin gives us textual identifier.
    -- Password check should occur on plugin side.
    -- 'authenticate' should perform one extra DB request (in our case)
    -- and return AuthId without password check.
    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate Creds{..} = do
        entity <- liftHandler . runDB $ select . from $
            \(u `LeftOuterJoin` e) -> do
                on (just (u ^. UserId) ==. (e ?. EmailUser))
                where_ ( u ^. UserIdent ==. val credsIdent )
                return u
        -- Password check is handled by auth plugin already
        return $ case entity of
            (Entity uid _:_) -> Authenticated uid
            []               -> UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [prizmJSONAuth]

    renderAuthMessage _ ("ru":_)   = russianMessage
    renderAuthMessage msg (_:rest) = renderAuthMessage msg rest
    renderAuthMessage _ []         = defaultMessage

    loginHandler :: AuthHandler App Html
    loginHandler = do
        ma <- maybeAuthId
        when (isJust ma) (redirect HomeR)
        authLayout [whamlet|_{MsgJSONApiOnly}|]

instance YesodAuthPersist App where
    type AuthEntity App = User
    getAuthEntity uid = liftHandler . runDB $ get uid


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ ("ru":_)   = russianFormMessage
    renderMessage msg (_:rest) = renderMessage msg rest
    renderMessage _ []         = defaultFormMessage


-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger


appNonce128urlT :: Handler Text
appNonce128urlT =
    (appNonceGen <$> getYesod)
    >>= liftIO . Nonce.nonce128urlT

setAppTitle :: [ AppMessage ] -> Widget
setAppTitle = setCompositeTitle . (:) MsgProjectName

setAppPageTitle :: AppMessage -> Widget
setAppPageTitle = setAppTitle . (: [])

allAccessRights  :: [ AccessType ]
allAccessRights = [ minBound .. maxBound ]

allAccessRightsJ :: I.Value
allAccessRightsJ = toJSON $ map
        (\ar -> let x = txt ar in AccessRightJ x x x)
        allAccessRights

-- ** React

addUiBundle :: Widget
addUiBundle =  do
    addStylesheet (StaticR js_ui_umi_css)
    addScript (StaticR js_ui_umi_js)
