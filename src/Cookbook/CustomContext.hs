{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Cookbook.CustomContext where

import           Control.Monad                             (void)
import           Control.Monad.IO.Class                    (liftIO)
import           Control.Monad.Reader
import           Data.Aeson                                (FromJSON (..),
                                                            ToJSON (..))
import qualified Data.Aeson                                as Aeson
import           Data.Default
import           Data.Proxy
import           Data.Text                                 (Text)
import           Data.Time.Clock                           (UTCTime,
                                                            getCurrentTime)
import           GHC.Generics
import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger      (mkRequestLogger)
import           Network.Wai.Middleware.RequestLogger.JSON
import           Servant
import qualified Servant.Auth                              as SAuth
import qualified Servant.Auth.Server                       as SAServer
import           Servant.Auth.Server.Internal.ConfigTypes  (CookieSettings)
import           System.Log.FastLogger                     (LoggerSet,
                                                            ToLogStr (..),
                                                            defaultBufSize,
                                                            flushLogStr,
                                                            newStdoutLoggerSet,
                                                            pushLogStrLn)

--- API Types ---

type CookieHeader = Headers '[ Header "Set-Cookie" SAServer.SetCookie, Header "Set-Cookie" SAServer.SetCookie ] ()
type AdminAPI = "admin" :> Get '[JSON] LogMsg
type LoginAPI = "login"
              :> ReqBody '[JSON] LoginForm
              :> Post '[JSON] CookieHeader
type API auths = (SAuth.Auth auths AdminUser :> AdminAPI) :<|> LoginAPI

data AppRes = Ok | Err
            deriving (Show, Generic)
instance ToJSON AppRes
instance FromJSON AppRes

newtype AdminUser = AdminUser
                  { usr :: Text
                  } deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance SAServer.ToJWT AdminUser
instance SAServer.FromJWT AdminUser

data LoginForm = LoginForm
               { username :: !Text
               , pass     :: !Text
               } deriving (Show, Generic)
instance ToJSON LoginForm
instance FromJSON LoginForm


--- Types ---

type App = ReaderT AppCtx Handler

data AppCtx = AppCtx
            { _getConfig :: AppConfig
            , _getLogger :: LoggerSet
            }

data AppConfig = AppConfig
               { env   :: !Text
               , ver   :: !Text
               , admin :: AuthAdmin
               }

data AuthAdmin = AuthAdmin
               { aUsername :: !Text
               , aPass     :: !Text
               }

data LogLevel
    = Debug
    | Info
    | Error
    | Critical
    | Alert
    deriving (Eq, Show, Generic)
instance ToJSON  LogLevel
instance FromJSON LogLevel

data LogMeta = LogMeta
             { _timestamp :: !UTCTime
             , _level     :: LogLevel
             , _ver       :: !Text
             , _env       :: !Text
             } deriving (Show, Eq, Generic)
instance ToJSON LogMeta
instance FromJSON LogMeta

data LogMsg = LogMsg
            { message  :: !Text
            , metadata :: LogMeta
            } deriving (Eq, Show, Generic)
instance ToJSON LogMsg
instance FromJSON LogMsg
instance ToLogStr LogMsg where
    toLogStr = toLogStr . Aeson.encode

--- API ---

runServer :: IO ()
runServer = do
  let appCfg = AppConfig "dev" "1.0.0" (AuthAdmin "admin" "admin")
  warpLogger <- jsonRequestLogger
  appLogger  <- newStdoutLoggerSet defaultBufSize
  myKey      <- SAServer.generateKey
  flushLogStr appLogger
  let ctx = AppCtx appCfg appLogger
      warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort port warpSettings
      settings     = Warp.setTimeout 50 portSettings
      jwtCfg       = SAServer.defaultJWTSettings myKey
      cookieCfg    = if env appCfg == "dev"
                       then SAServer.defaultCookieSettings { SAServer.cookieIsSecure = SAServer.NotSecure }
                       else SAServer.defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext
  Warp.runSettings settings $ warpLogger $ mkApp cfg cookieCfg jwtCfg ctx


mkApp :: Context '[SAServer.CookieSettings, SAServer.JWTSettings ]
      -> SAServer.CookieSettings
      -> SAServer.JWTSettings
      -> AppCtx
      -> Application
mkApp cfg cs jwts appCtx = do
    let api    = Proxy :: Proxy (API '[SAServer.JWT])
        allApi = Proxy :: Proxy '[ SAServer.CookieSettings, SAServer.JWTSettings ]
    serveWithContext api cfg $
      hoistServerWithContext api allApi (flip runReaderT appCtx) $ adminServer cs jwts

adminServer :: SAServer.CookieSettings -> SAServer.JWTSettings -> ServerT (API auths) App
adminServer cs jwts = adminHandler :<|> loginHandler cs jwts

--- API Handlers ---

jsonRequestLogger :: IO Middleware
jsonRequestLogger = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

adminHandler :: SAServer.AuthResult AdminUser -> App LogMsg
adminHandler (SAServer.Authenticated (AdminUser usr)) = logHandler ("Admin User access: " <>  usr, Debug)
adminHandler _  = throwError err401

loginHandler :: SAServer.CookieSettings
             -> SAServer.JWTSettings
             -> LoginForm
             -> App (Headers '[Header "Set-Cookie" SAServer.SetCookie, Header "Set-Cookie" SAServer.SetCookie] ()) -- CookieHeader
loginHandler cookieSettings jwtSettings form = do
   AppConfig _ _ admin <- asks _getConfig
   case validateLogin form admin of
     Nothing -> do
       void $ logHandler ("Admin login failed for: " <> username form, Error)
       throwError err401
     Just usr -> do
       maybeCookie <- liftIO $ SAServer.acceptLogin cookieSettings jwtSettings usr
       case maybeCookie of
         Nothing ->
           -- void $ logHandler ("Admin login failed for: " <> username form, Error)
           throwError err401
         Just appCookie ->
           -- void $ logHandler ("Admin login successful: " <> username form, Info)
           pure $ appCookie ()
loginHandler _ _ _ = throwError err401

validateLogin :: LoginForm -> AuthAdmin -> Maybe AdminUser
validateLogin (LoginForm usr pass) (AuthAdmin aUsr aPass)
  | usr == aUsr && pass == aPass = Just $ AdminUser usr
  | otherwise = Nothing

--- Helpers ---

logHandler :: (Text, LogLevel) -> App LogMsg
logHandler (msg, logLevel) = do
   (AppCtx appCfg appLog) <- ask
   liftIO $ do
     curTime <- getCurrentTime
     let logMeta = mkLogMeta (curTime, logLevel, appCfg)
         logMsg  = LogMsg msg logMeta
     pushLogStrLn appLog $ toLogStr logMsg
     pure logMsg
  where
    mkLogMeta (curTime, logLv, AppConfig {..})
        = LogMeta
        { _timestamp = curTime
        , _level = logLv
        , _env   = env
        , _ver   = ver
        }

port :: Int
port = 3001
