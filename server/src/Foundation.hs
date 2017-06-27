{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Utils.AccessToken
import Yesod.Auth.OpenId (IdentifierType(Claimed), authOpenId)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

import Utils.Order

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings :: AppSettings
    , appStatic :: Static -- ^ Settings for static file serving.
    , appConnPool :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

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

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
                                                                      where
    approot =
        ApprootRequest $ \app req ->
            case appRoot $ appSettings app of
                Nothing -> getApprootText guessApproot app req
                Just root -> root
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ =
        Just <$>
        defaultClientSessionBackend
            120 -- timeout in minutes
            "config/client_session_key.aes"
    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute
        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs
        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $
                  MenuItem
                  { menuItemLabel = "Home"
                  , menuItemRoute = HomeR
                  , menuItemAccessCallback = True
                  }
                , NavbarLeft $
                  MenuItem
                  { menuItemLabel = "Profile"
                  , menuItemRoute = ProfileR
                  , menuItemAccessCallback = isJust muser
                  }
                , NavbarLeft $
                  MenuItem
                  { menuItemLabel = "Create Item"
                  , menuItemRoute = CreateItemR
                  , menuItemAccessCallback = isJust muser
                  }
                , NavbarLeft $
                  MenuItem
                  { menuItemLabel = "Create Order"
                  , menuItemRoute = CreateOrderR
                  , menuItemAccessCallback = isJust muser
                  }
                , NavbarRight $
                  MenuItem
                  { menuItemLabel = "Login"
                  , menuItemRoute = AuthR LoginR
                  , menuItemAccessCallback = isNothing muser
                  }
                , NavbarRight $
                  MenuItem
                  { menuItemLabel = "Logout"
                  , menuItemRoute = AuthR LogoutR
                  , menuItemAccessCallback = isJust muser
                  }
                ]
        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
        let navbarLeftFilteredMenuItems =
                [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems =
                [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <-
            widgetToPageContent $ do
                addStylesheet $ StaticR semantic_semantic_min_css
                addScript $ StaticR semantic_sidebar_min_js
                addScript $ StaticR semantic_transition_min_js
                addScript $ StaticR semantic_visibility_min_js
                $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR (StaticRoute ["item-pdf", filename] [])) _ = do
        mauth <- maybeAuthPair
        case mauth of
            Nothing -> return AuthenticationRequired
            Just (userId, _) -> do
                hasAccess <- hasAccessToPdfFileDownload userId filename
                admin <- isAdmin
                return $
                    if hasAccess == Authorized || admin == Authorized
                        then Authorized
                        else hasAccess
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized ProfileR _ = isAuthenticated
    isAuthorized LoginTokenR _ = isAuthenticated
    isAuthorized (RegenerateAccessTokenR uid) _ = isOwnerOrAdmin uid
    -- @todo: Fix access
    isAuthorized (ItemR _) _ = return Authorized
    isAuthorized CreateItemR _ = isAuthenticated
    isAuthorized (EditItemR _) _ = isAuthenticated
    isAuthorized (RestfulItemR _ _) _ = isAuthenticated
    isAuthorized (RestfulItemsR _) _ = isAuthenticated
    isAuthorized (PdfFileR _) _ = isAuthenticated
    isAuthorized PdfFileCreateR _ = isAuthenticated
    -- Orders
    isAuthorized RestfulOrderR _ = return Authorized
    isAuthorized (RestfulOrderItemR _) _ = isAuthenticated
    isAuthorized CreateOrderR _ = isAuthenticated
    isAuthorized (OrderR _) _ = isAuthenticated
    isAuthorized (EditOrderR _) _ = isAuthenticated
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
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
        -- Generate a unique filename based on the content itself
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs
    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app) ||
        level == LevelWarn || level == LevelError
    makeLogger = return . appLogger
    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR _) = return ("Login", Just HomeR)
    breadcrumb ProfileR = return ("Profile", Just HomeR)
    breadcrumb _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId
    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True
    authenticate creds =
        runDB $ do
            x <- getBy $ UniqueUser $ credsIdent creds
            case x of
                Just (Entity uid _) -> return $ Authenticated uid
                Nothing -> do
                    uid <-
                        insert
                            User
                            { userIdent = credsIdent creds
                            , userPassword = Nothing
                            }
              -- Create access token for the new user.
                    accessTokenText <- generateToken
                    currentTime <- liftIO getCurrentTime
                    _ <- insert $ AccessToken currentTime uid accessTokenText
                    return $ Authenticated uid
    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins app = [authOpenId Claimed []] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
      where
        extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]
    authHttpManager = getHttpManager

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $
        case muid of
            Nothing -> Unauthorized "You must login to access this page"
            Just _ -> Authorized

-- | Access function to determine if a user has admin role.
isAdmin :: Handler AuthResult
isAdmin = do
    muid <- maybeAuthId
    case muid of
        Nothing -> return $ Unauthorized "You must login to access this page"
        Just uid
          -- Check if user has the "admin" role.
         -> do
            mRole <- runDB $ selectFirst [RoleName ==. "admin"] []
            case mRole of
                Nothing ->
                    return $
                    Unauthorized "admin role does not defined in the site"
                Just role -> do
                    mUserRole <-
                        runDB $
                        selectFirst
                            [ UserRoleUser ==. uid
                            , UserRoleRole ==. (entityKey role)
                            ]
                            []
                    return $
                        case mUserRole of
                            Nothing ->
                                Unauthorized
                                    "You must be an admin to access this page"
                            Just _ -> Authorized

-- | Access function to determine if a current user is the owner or has admin role.
isOwnerOrAdmin :: Key User -> Handler AuthResult
isOwnerOrAdmin uid = do
    mCurrentUid <- maybeAuthId
    case mCurrentUid of
        Nothing -> return $ Unauthorized "You must login to access this page"
        Just currentUid ->
            if (currentUid == uid)
                then return Authorized
                else isAdmin

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
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
