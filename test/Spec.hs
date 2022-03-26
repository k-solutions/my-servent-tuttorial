{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import           Lib
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /users?sortBy" $ do
      it "reponds with 200" $ do
        get "/users?sortBy=id" `shouldRespondWith` 200
      it "responds with [User]" $ do
        get "/users?sortBy=name" `shouldRespondWith` [json|[
           { userId: 2
           , userFirstName: "Albert"
           , userLastName: "Einstein"
           }, {userId: 1,
            userFirstName: "Isaac",
            userLastName: "Newton"
           }                                     ]
                                                    |]

    describe "GET /position/x/y" $ do
      it "responds with 200" $ do
        get "/position/1/2" `shouldRespondWith` 200
      it "respond with Position" $ do
          get "/position/1/2" `shouldRespondWith` [json|{posX: 1, posY: 2}|]

    describe "GET /newton" $ do
        it "responds with 200" $ do
            get "/newton" `shouldRespondWith` 200
        it "responds with User" $ do
            let newton = "{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"}"
            get "/newton" `shouldRespondWith` [json|{userId: 1, userFirstName: "Isaac", userLastName: "Newton"}|]


    describe "GET /users" $ do
        it "users responds with 200" $ do
          get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
          get "/users" `shouldRespondWith` [json|
          [{userId: 1,
            userFirstName: "Isaac",
            userLastName: "Newton"
          },{ userId: 2
            , userFirstName: "Albert"
            , userLastName: "Einstein"
          }]
        |]

