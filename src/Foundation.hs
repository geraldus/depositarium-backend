{-# LANGUAGE CPP                   #-}
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

import           Import.NoFoundation
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
import           Type.App
import           Utils.Common

import qualified Crypto.Nonce            as Nonce
import           Data.Aeson              ( encode )
import           Data.List               ( isSubsequenceOf )
import qualified Database.Esqueleto      as E
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
        master <- getYesod
        user <- maybeAuthPair
        route <- getCurrentRoute
        menues <- appMenuItems user route
        routeRender <- getUrlRender
        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        idApp <- newIdent
        (_title, _parents) <- breadcrumbs
        msg <- getMessageRender
        let accessRightsJSON = encodeStrictText allAccessRightsJ
        pc <- widgetToPageContent $ do
            addReactScripts
            addReactBundle
            $(widgetFile "default-layout")
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
                selectList [ UserRightsUser ==. user ] []


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

instance PrizmAuthPlugin App

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
    --
    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate Creds{..} = case credsPlugin of
        "PRIZM Yesod Auth Plugin" -> do
            entity <- liftHandler . runDB $ E.select . E.from $
                \(u `E.LeftOuterJoin` e) -> do
                    E.on (E.just (u E.^. UserId) E.==. (e E.?. EmailUser))
                    E.where_
                        (       u E.^. UserIdent E.==. E.val credsIdent
                          E.||. e E.?. EmailEmail E.==. E.just (E.val credsIdent) )
                    return u
            return $ case entity of
                (Entity uid _:_) -> Authenticated uid
                []               -> UserError InvalidLogin
        -- FIXME: Provide better error when plugin name not recognized
        _ -> return $ UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authPrizm]

    renderAuthMessage _ ("ru":_)   = russianMessage
    renderAuthMessage msg (_:rest) = renderAuthMessage msg rest
    renderAuthMessage _ []         = defaultMessage

    -- authLayout :: Widget -> Handler Html
    authLayout widget =
        liftHandler . defaultLayout $ do
            msg <- liftHandler getMessage
            [whamlet|
                $maybe m <- msg
                    <div .alert>#{m}
                <script>const NO_REACT = true|]
            widget

    loginHandler :: AuthHandler App Html
    loginHandler = do
        ma <- maybeAuthId
        tp <- getRouteToParent
        when (isJust ma) (redirect HomeR)
        authLayout $ do
            setAppPageTitle MsgSignInPageTitle
            [whamlet|
                <div .container>
                    <div .row .justify-content-center>
                        <div .col-11 .col-md-6>
                            <form pb-5 action=@{tp Local.Auth.loginR} method=post>
                                <h2 .text-center>_{MsgSignInPageTitle}
                                <div .form-group>
                                    <label for="email-in">_{MsgEmailAddress}
                                    <input type=text .form-control #email-in placeholder="email@domain.com" name="username">
                                <div .form-group>
                                    <label for="pass-in">_{MsgPassword}
                                    <input type=password .form-control #pass-in placeholder="******" name="password">
                                <div .form-group .row>
                                    <div .col-12 .col-sm-9 col-md-6 .mx-auto>
                                        <button .btn.btn-lg.btn-block.btn-outline-primary type=submit>войти
                |]
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

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding


fsAddPlaceholder :: FieldSettings App -> Text -> FieldSettings App
fsAddPlaceholder settings p = let
        attrs = fsAttrs settings ++ [("placeholder", p)]
    in settings { fsAttrs = attrs }

fsAddClasses :: FieldSettings App -> [ Text ] -> FieldSettings App
fsAddClasses settings cs = let
        attrs = fsAttrs settings
        csi = unwords cs
        attrs' = updateAttrs attrs [] csi
    in settings { fsAttrs = attrs' }
    where
        updateAttrs [] acc classes = acc ++ [ ("class", classes) ]
        updateAttrs (a@(aname, aval):rest) acc classes
            | aname == "class" = acc ++ [ ("class", aval <> " " <> classes) ] ++ rest
            | otherwise = updateAttrs rest (acc ++ [ a ]) classes

fsAddAttrs :: [ ( Text, Text) ] -> FieldSettings App -> FieldSettings App
fsAddAttrs attrs settings =
    let as = fsAttrs settings
    in settings { fsAttrs = as <> attrs }

fsBs4 :: FieldSettings App
fsBs4 = fsWithClasses [ "form-control" ] "" Nothing Nothing Nothing []

fsBs4WithId :: Text -> FieldSettings App
fsBs4WithId ident = fsWithClasses
    [ "form-control" ] "" Nothing (Just ident) Nothing []

fsWithClasses
    :: [ Text ]
    -> SomeMessage App
    -> Maybe (SomeMessage App)
    -> Maybe Text
    -> Maybe Text
    -> [ ( Text, Text ) ]
    -> FieldSettings App
fsWithClasses classList lbl tlt mid mnam attrs =
    let cs = unwords classList
        as = attrs <> [ ( "class", cs ) ]
    in FieldSettings lbl tlt mid mnam as


appNonce128urlT :: Handler Text
appNonce128urlT =
    (appNonceGen <$> getYesod)
    >>= liftIO . Nonce.nonce128urlT

setAppTitle :: [ AppMessage ] -> Widget
setAppTitle = setCompositeTitle . (:) MsgProjectName

setAppPageTitle :: AppMessage -> Widget
setAppPageTitle = setAppTitle . (: [])

appMenuItems
    :: Maybe (UserId, User)
    -> Maybe (Route App)
    -> Handler [ MenuGroup App ]
appMenuItems user _ = do
    msg <- getMessageRender
    accessRights <- case user of
        Nothing     -> pure [ ]
        Just (u, _) ->
            map entityVal
            <$> runDB (selectList [ UserRightsUser ==. u ] [ ])
    return $
        [ SingleItem (itemHome msg) cl pl
        ]
        <> userItems msg user (plainAccess accessRights)
  where
    userItems
        :: (AppMessage -> Text)
        -> Maybe (UserId, User)
        -> [ AccessType ]
        -> [ MenuGroup App ]
    userItems msg Nothing _ = guestItems msg
    userItems msg (Just (_, u)) access =
        let manageItems = manageUserItems msg access
            loggedItems =  loggedUserItems msg
            operatorItems = operatorUserItems msg access
            manage = [ ItemGroup manageItems st pr (msg MsgManageUserMenuTitle) ]
            operator = [ ItemGroup operatorItems st pl (msg MsgProfileViewMenuTitle) ]
            logged = [ ItemGroup loggedItems st pr (userIdent u) ]
        in cleanGroup $ manage <> operator <> logged


    guestItems :: (AppMessage -> Text) -> [ MenuGroup App ]
    guestItems msg = [ SingleItem (itemSignIn msg) st pr ]

    manageUserItems :: (AppMessage -> Text) -> [ AccessType ] -> [ MenuItem App ]
    manageUserItems msg access =
        let listUsers = [itemListUsers msg | ListUsers `elem` access]
            createUser = [itemCreateUser msg | CreateUser `elem` access]
        in listUsers <> createUser <> [ ]

    operatorUserItems :: (AppMessage -> Text) -> [ AccessType ] -> [ MenuItem App ]
    operatorUserItems msg access =
        let viewProfile = [itemOperatorProfileView msg | SelfBalanceView `elem` access]
        in viewProfile <> [ ]

    loggedUserItems msg = [ itemSignOut msg ]

    itemHome msg = MenuItem (msg MsgHomePageTitle) HomeR

    itemSignIn msg = MenuItem (msg MsgSignInPageTitle) (AuthR LoginR)
    itemSignOut msg = MenuItem (msg MsgSignOut) (AuthR LogoutR)

    itemListUsers msg = MenuItem (msg MsgListUserPageTitle) ManageListUserR
    itemCreateUser msg = MenuItem (msg MsgCreateUserPageTitle) ManageCreateUserR

    itemOperatorProfileView msg
        = MenuItem (msg MsgProfileViewMenuTitle) OperatorProfileViewR

    cleanGroup :: [MenuGroup App] -> [MenuGroup App]
    cleanGroup gs = reverse $ step [] gs
        where step acc []                        = acc
              step acc (ItemGroup [] _ _ _:rest) = step acc rest
              step acc (x:rest)                  = step (x:acc) rest

    plainAccess = map userRightsAccess

    cl = MTCollapsible
    st = MTSticky
    pl = MPLeft
    -- pc = MPCenter
    pr = MPRight

allAccessRights  :: [ AccessType ]
allAccessRights = [ minBound .. maxBound ]

allAccessRightsJ :: Value
allAccessRightsJ = toJSON $ map
        (\ar -> let x = txt ar in (AccessRightJ x x x))
        allAccessRights

-- ** React

addReactScripts :: Widget
addReactScripts = do
    addScriptRemote $
        pkgHost "react" <> "." <> reactBuild <> ".js"
    addScriptRemote $
        pkgHost "react-dom" <> "." <> reactBuild <> ".js"
  where pkgHost pkg = "https://unpkg.com/" <> pkg <> "@16/umd/" <> pkg

addReactBundle :: Widget
addReactBundle =  addScriptAttrs (StaticR js_bundle_js) []

reactBuild :: Text
reactBuild =
#ifdef DEVELOPMENT
    "development"
#else
    "production.min"
#endif
