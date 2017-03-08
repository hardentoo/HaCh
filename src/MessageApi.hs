{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}

module MessageApi(server, API) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.String.Conversions
import Control.Monad.Logger (runStderrLoggingT)

import Data.List (find)

import Models

{-
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message
    heading String Maybe
    text String
    likes Int
    deriving Eq Show
|]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \ v ->
    Message <$> v .: "heading"
            <*> v .: "text"
            <*> pure 0

instance ToJSON Message where
  toJSON m =
    object [ "heading" .= messageHeading m
           , "text"    .= messageText m
           , "likes"   .= messageLikes m
           ]
-}


type API = "new"
          :> ReqBody '[JSON] Message
          :> Post '[JSON] (Key Message)
      :<|> "msg" :> "get"
          :> Capture "id" (Key Message)
          :> Get '[JSON] (Maybe (Entity Message))
      :<|> "all"
          :> Get '[JSON] [(Entity Message)]
      :<|> "like"
          :> Capture "id" (Key Message)
          :> Post '[JSON] String

server :: ConnectionPool -> Server API
server pool = newMsg
         :<|> getMsg
         :<|> allMsgs
         :<|> likeMsg

  where getMsg mid = liftIO $ flip runSqlPersistMPool pool $ do
          msg <- selectFirst [MessageId ==. mid] []
          return $ msg

        newMsg msg = liftIO $ flip runSqlPersistMPool pool $ do
          insert msg

        allMsgs = liftIO $ flip runSqlPersistMPool pool $ do
          msgs <- selectList [] []
          return $ msgs

        likeMsg mid = liftIO $ flip runSqlPersistMPool pool $ do
          msg <- get mid
          case msg of
            Just r -> do
              let curr = messageLikes r
              update mid [MessageLikes =. curr + 1]
              return "Ok"
            Nothing -> return "Not found"

api :: Proxy API
api = Proxy

{-
app = do
  pool <- getConnectionPool "sqlite.db"
  return $ serve api (server pool)
-}
