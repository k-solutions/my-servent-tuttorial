module Cookbook.SqlLiteAPI 
  ( runApp
  , initDb
  , msgServer
  , PersistAPI
  ) where

import Control.Concurrent
import Control.Exception (bracket)
import Database.SQLite.Simple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Data.Aeson
import Data.Proxy
import Control.Monad.IO.Class (liftIO)

newtype Msg = Msg { msgText :: String } deriving (Show, Generic)
instance FromJSON Msg
instance ToJSON Msg     

type PersistAPI = ReqBody '[JSON] Msg  :> Post '[JSON] NoContent
                :<|> Get '[JSON] [Msg]

api :: Proxy PersistAPI
api = Proxy

initDb :: FilePath -> IO ()
initDb dbFile = withConnection dbFile $ \conn ->
  execute_ conn "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

msgServer :: FilePath -> Server PersistAPI
msgServer dbFile = postMsgHandler :<|> getMsgsHandler
  where
    postMsgHandler :: Msg -> Handler NoContent
    postMsgHandler Msg { msgText = msgTxt } = do
      liftIO . withConnection dbFile $ \conn ->
        execute conn "INSERT INTO messages VALUES (?)" (Only msgTxt)
      pure NoContent

    getMsgsHandler :: Handler [Msg]
    getMsgsHandler = do
      let sqlQuery conn = query_ conn "SELECT msg FROM messages" :: IO [Only String] 
      msgRes <- liftIO $ withConnection dbFile sqlQuery
      mapM (pure . Msg . fromOnly) msgRes 
                  
runApp :: FilePath -> IO ()
runApp dbFile = run 8080 (serve api $ msgServer dbFile)
