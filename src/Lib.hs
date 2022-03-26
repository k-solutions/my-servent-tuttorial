{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}

module Lib
    ( startApp
    , app
    , User
    , Position
    , Msg
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString                  (ByteString)
import           Data.Data                        (Typeable)
import           Data.List                        (find, sortBy)
import           Data.Map                         (Map (..))
import qualified Data.Map                         as Map
import           Data.Text                        (Text, pack, unpack)
import           Data.Text.Encoding               (decodeUtf8)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Experimental.Auth 
import           Web.Cookie (parseCookies)

-- | Application Types

newtype Account = Account { unAccount :: Text }

data User = User
  { userId        :: Int
  , userEmail     :: Text
  , userFirstName :: Text
  , userLastName  :: Text
  , userPassword  :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data Position = Position
              { posX :: Int
              , posY :: Int
              } deriving (Show, Generic)
instance ToJSON Position

newtype Msg = Msg
            { msg :: String
            } deriving (Show, Generic)
instance ToJSON Msg

-- | Public API
type PublicAPI = Get '[JSON] Position

-- | Private API
type PrivateAPI = Get '[JSON] [User]

-- | Application API
type API = "users" :> AuthProtect "cookie-auth" :> QueryParam "sortBy" SortBy :> PrivateAPI
         :<|> "position" :> Capture "x" Int :> Capture "y"  Int :> PublicAPI

data SortBy = Id | Name

instance FromHttpApiData SortBy where
    parseUrlPiece arg = case unpack arg of
      "id"   -> Right Id
      "name" -> Right Name
      _      -> Left $ pack "Unkown SortBy field"

type instance AuthServerData (AuthProtect "cookie-auth") = Account 

--- API ---

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serveWithContext api genAuthServerContext server

api :: Proxy API
api = Proxy

server :: Server API
server =
    let positionHandler x y = pure $ Position x y
        usersHandler (Account _acc) = pure . sortedUsers 
        --- usersHandler (_user :: User) = pure . sortedUsers
    in usersHandler
       :<|> positionHandler

--- Helpers ----

--- Account Helpers ---

-- | The context will be made available to "cookie-auth"  tagged requests
-- so that AuthProtect  can extract the handler and run it on request
genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext

-- | The auth handler to wrap Request -> Handler Account
-- We expect request headers to be in the cookie
authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401  msg   = throwError (err401 {errBody = msg})
    handler req     = either throw401 accountLookup $ do
      cookies <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookies

accountLookup :: ByteString -> Handler Account
accountLookup key =
       case Map.lookup key accDatabase of
         Nothing  -> throwError (err403 { errBody = "Invalid key" })
         Just usr -> pure usr

accDatabase :: Map ByteString Account
accDatabase = Map.fromList [ ("key1", Account "Anne Briggs")
                           , ("key2", Account "Bruce Cockburn")
                           , ("key3", Account "Ghédalia Tazartès")
                           ]

-- | Right context for the Handlers - BasicAuthCheck value tag with "users"
basicAuthServerContext :: Context (BasicAuthCheck User ': ' [])
basicAuthServerContext = authCheck :. EmptyContext

authCheck :: BasicAuthCheck User
authCheck =
    let check (BasicAuthData  usrEmail usrPass) = case findUserBy (decodeUtf8 usrEmail, decodeUtf8 usrPass) of
          Just usr -> pure (Authorized  usr)
          Nothing  -> pure Unauthorized
    in BasicAuthCheck check

findUserBy :: (Text,Text) -> Maybe User
findUserBy authParams = find (hasUsrEmailAndPass authParams) users
  where
    hasUsrEmailAndPass :: (Text, Text) -> User -> Bool
    hasUsrEmailAndPass (usrEmail, usrPass) User {..} = usrEmail == userEmail && usrPass == userPassword

sortedUsers :: Maybe SortBy -> [User]
sortedUsers Nothing = users
sortedUsers (Just toBeSorted) = flip sortBy users $
  case toBeSorted of
    Id   -> compareBy userId
    Name -> compareBy userFirstName

compareBy :: Ord a => (User -> a) -> User -> User -> Ordering
compareBy f x1 x2 = compare (f x1) (f x2)

newton :: User
newton =  User 1 "isaac@test.com" "Isaac" "Newton" "test"

users :: [User]
users  = [ newton
         , User 2 "albert@test.com" "Albert" "Einstein" "test"
         ]
