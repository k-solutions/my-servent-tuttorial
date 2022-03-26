{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Cookbook.StructAPI
  ( startApp
  , productServer
  , userServer
  , BaseAPI
--  , User
--  , UserId
  ) where

-- import           Data.Aeson
-- import           GHC.Generics
import           GHC.TypeLits
import           Network.Wai.Handler.Warp
import           Servant
-- import           Servant.Elm              (defaultOptions, deriveBoth)
import           Cookbook.Types
import qualified Data.Text                as Text (pack)
import           Servant.Elm              (DefineElm (DefineElm),
                                           ElmOptions (urlPrefix),
                                           Proxy (Proxy), UrlPrefix (Static),
                                           defElmImports, defElmOptions,
                                           generateElmForAPIWith,
                                           generateElmModuleWith)

--- API ---
port :: Int
port = 8000

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static $ "http://localhost" <> ":" <> (Text.pack . show) port }

genElmClient :: IO ()
genElmClient = generateElmModuleWith
                myElmOpts
                [ "Generated"
                , "BooksApi"
                ]
                defElmImports "elm" [ DefineElm (Proxy :: Proxy User)
                                    , DefineElm (Proxy :: Proxy Product)
                                    ]
                (Proxy :: Proxy API)

startApp :: IO ()
startApp = run port $ serve api server
  where
    api = Proxy :: Proxy API
    server = factoringServer :<|> userServer :<|> productServer

userServer :: Server (BaseAPI "user" User UserId)
userServer = baseServer userLst getUser postUser
  where
    userLst = pure []
    getUser userId = pure $
      if userId == 1
        then User "One" 51
        else User "Anonimius" 0
    postUser _user = return NoContent

productServer :: Server (BaseAPI "product" Product ProductId)
productServer = baseServer productLst getProduct postProduct
  where
    productLst = return []
    postProduct _prod = return NoContent
    getProduct _prodId = pure $ Product "My Presios"

--- Base API ---

type API = FactoringAPI
         :<|> BaseAPI "users" User UserId
         :<|> BaseAPI "products" Product ProductId


-- | SimpleAPI has three endpoints:
--     - GET /<name>
--     - GET /<name>/<some 'i'>
--     - POST /<name>
type BaseAPI (name :: Symbol) a i = name :>
  ( Get '[JSON] [a]
  :<|> Capture "id" i :> Get '[JSON] a
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )

baseServer
  :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (BaseAPI name a i)
baseServer listAs getA postA = listAs :<|> getA :<|> postA

--- Factoring ---

-- | 2 Endpoints:
--     GET /x/<some 'Int'>[?y=<some 'Int'>]
--     POST /x/<some 'Int'>
type FactoringAPI = "x" :> Capture "x" Int :>
                        ( QueryParam "y" Int :> Get '[JSON] Int
                        :<|>                    Post '[JSON] Int
                        )

factoringServer :: Server FactoringAPI
factoringServer x = getXY :<|> postX
  where
    getXY Nothing  = pure x
    getXY (Just y) = pure (x + y)
    postX          = pure (x - 1)

