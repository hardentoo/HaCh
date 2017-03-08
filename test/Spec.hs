{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON


main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do

    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},\
                \{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"},\
                \{\"userId\":3,\"userFirstName\":\"Bertrand\",\"userLastName\":\"Russell\"},\
                \{\"userId\":4,\"userFirstName\":\"Gottlob\",\"userLastName\":\"Frege\"}]"
            get "/users" `shouldRespondWith` users
        it "responds with 200 JSON" $ do
            get "/users" `shouldRespondWith` 200 { matchHeaders = ["Content-Type" <:> "application/json"] }


    describe "GET /logicians" $ do
        it "responds with 200" $ do
            get "/logicians" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let logicians = "[{\"userId\":3,\"userFirstName\":\"Bertrand\",\"userLastName\":\"Russell\"},\
                \{\"userId\":4,\"userFirstName\":\"Gottlob\",\"userLastName\":\"Frege\"}]"
            get "/logicians" `shouldRespondWith` logicians
