{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module ApiServer
  ( startApp
  , port
  , AppAPI
  , TestAPI
  ) where

import           Control.Monad.IO.Class   (liftIO)
import           Cookbook.SqlLiteAPI
import           Cookbook.StructAPI       hiding (startApp)
import           Cookbook.Types
import           Data.Aeson
import           Data.ByteString          (ByteString)
import           Data.Map                 as Map
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp
import           Servant                  (BasicAuthData)
import           Servant                  as Srv
import           Servant.Auth             as SrvAuth
import           Servant.Auth.Server      as SASrv
import           Servant.Client
import           System.IO

--- Settings ---

port :: Int
port = 3001

sqlLiteDb :: FilePath
sqlLiteDb = "./test.sql"

startApp :: IO ()
startApp = do
    connPool <- initConnPool
    _ <- initDb sqlLiteDb
    let settings = setPort port
                 $ setBeforeMainLoop (hPutStrLn stderr $ "listening on port " <> show port) defaultSettings
    runSettings settings =<< mkApp sqlLiteDb connPool

--- Server API ---

type TestAPI =    "foo" :> Capture "i" Int :> Get '[JSON] ()
             :<|> "bar" :> Get '[JSON] ()
             :<|> BaseAPI "users" User UserId
             :<|> PersistAPI

type TestAPIProtected = Auth '[SrvAuth.JWT, SrvAuth.BasicAuth] AuthUser :> TestAPI
type AppAPI = TestAPIProtected

protectedServer :: FilePath -> Server TestAPIProtected
protectedServer dbFile (Authenticated user) = fooHandle
                                            :<|> barHandle
                                            :<|> userServer
                                            :<|> msgServer dbFile
  where
    fooHandle :: Int -> Handler ()
    fooHandle  n = liftIO $ hPutStrLn stderr $ concat ["foo: ", show user, "/", show n]
    barHandle :: Handler ()
    barHandle = liftIO testClient
protectedServer _ _  = throwAll err401 --- | catch all BadPassword, NoSuchUser, Indefinite

mkApp :: FilePath -> Pool DbConn -> IO Application
mkApp dbFile connPool = do
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        authCfg = authCheck connPool
        allCfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
        api = Proxy :: Proxy AppAPI
        -- appServer = protectedServer :<|> userServer
    pure $ serveWithContext api allCfg $ protectedServer dbFile

--- Client ---

type TestAPIClient = Srv.BasicAuth "test" AuthUser :> TestAPI

testClient :: IO ()
testClient = do
    mgr <- newManager defaultManagerSettings
    let (foo :<|> _) = client (Proxy :: Proxy TestAPIClient) (BasicAuthData "name" "pass")
    res <- runClientM (foo 42) (mkClientEnv mgr (BaseUrl Http "localhost" port ""))
    hPutStrLn stderr $ case res of
                          Left err -> "Error: " <> show err
                          Right r  -> "Success: " <> show r

--- Auth Types ---

data AuthUser = AuthUser
              { auId    :: Int
              , auOrgId :: Int
              } deriving (Show, Eq, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser

--- DB Types ---
type Login = ByteString
type Pass  = ByteString
type AuthParms = (Login, Pass)
type DB    = Map AuthParms AuthUser
type Pool a = a
type DbConn = DB

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthUser)
instance FromBasicAuthData AuthUser where
  fromBasicAuthData authData authCheckFun = authCheckFun authData

--- Fake DB Data ---

allUsers :: [(AuthParms, AuthUser)]
allUsers = [ (("user1", "pass1"), AuthUser 1 1)
           , (("user2", "pass2"), AuthUser 2 1)
           ]

initConnPool :: IO (Pool DbConn)
initConnPool= pure . Map.fromList $ allUsers

--- Helpers ---

authCheck :: Pool DbConn
          -> BasicAuthData
          -> IO (AuthResult AuthUser)
authCheck connPool (BasicAuthData login pass)
  = pure
  $ maybe SASrv.Indefinite Authenticated
  $ Map.lookup (login, pass) connPool
