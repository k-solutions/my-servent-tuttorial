{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cookbook.DbPool where

import           Control.Concurrent
import           Control.Exception                    (bracket)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString                      (ByteString)
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.ToField   (ToField (..))
import           GHC.Generics
import           Network.HTTP.Client                  (defaultManagerSettings,
                                                       newManager)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant                              (NoContent (NoContent))
import           Servant.Client
--- Types ---

type DbConnString = ByteString
newtype Msg = Msg String
              deriving (Show, Generic, ToField, FromField, FromJSON, ToJSON)
type API =    ReqBody '[JSON] Msg :> Post '[JSON] NoContent
         :<|> Get '[JSON] [Msg]

--- Public API ---

dbConnStr = "postgresql://test:test@localhost/test"

startApp :: IO ()
startApp = do
    dbPool <- initConnPool dbConnStr
    initDb dbConnStr
    runApp dbPool

runApp :: Pool Connection -> IO ()
runApp = run 8000
       . serve api
       . server

--- Helpers ---

server :: Pool Connection -> Server API
server connPool = postMsg :<|> getMsgs
  where
    postMsg :: Msg -> Handler NoContent
    postMsg msg = liftIO (sqlInsert msg connPool) >> pure NoContent

    sqlInsert :: Msg -> Pool Connection -> IO ()
    sqlInsert msg connPool = do
        putStrLn $ "Recived msg: " <> show msg
        r <- withResource connPool $ \conn ->
              execute conn "insert into msgs values (?)" (Only msg)
        putStrLn $ "Inserted rows: " <> show r
        pure ()

    getMsgs :: Handler [Msg]
    getMsgs = fmap (map fromOnly) . liftIO $
      withResource connPool $ \conn ->
        query_ conn "select msg from msgs"

api :: Proxy API
api = Proxy

initConnPool :: DbConnString -> IO (Pool Connection)
initConnPool connStr = createPool (connectPostgreSQL connStr)
                                  close
                                  2       -- ^ Stipes
                                  60      -- ^ unused connections are kept
                                  10      -- ^ max connection open per stripe

initDb :: DbConnString -> IO ()
initDb dbConnStr = bracket (connectPostgreSQL dbConnStr) close $ \conn -> do
  execute_ conn "create table IF NOT EXISTS msgs (msg text not null)"
  pure ()


