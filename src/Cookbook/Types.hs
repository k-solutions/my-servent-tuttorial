{-# LANGUAGE TemplateHaskell #-}

module Cookbook.Types where

import           Servant.Elm (defaultOptions, deriveBoth)

--- Types ---

type UserId = Int
type ProductId = Int

data User = User
          { usrName :: String
          , usrAge  :: Int
          } deriving (Show) -- Generic
deriveBoth defaultOptions ''User

-- instance FromJSON User
-- instance ToJSON User

newtype Product = Product
             { prdName        :: String
        --     , prdDescription :: String
             } deriving (Show)  --- Generic
deriveBoth defaultOptions ''Product

-- instance FromJSON Product
-- instance ToJSON Product


