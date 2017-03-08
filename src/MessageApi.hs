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

module MessageApi
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.String.Conversions
import           Control.Monad.Logger (runStderrLoggingT)


import Data.List (find)

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

type API = "msg" :> "new"
          :> ReqBody '[JSON] Message
          :> Post '[JSON] (Key Message)
      :<|> "msg" :> "get"
          :> Capture "id" (Key Message)
          :> Get '[JSON] (Maybe Message)
      :<|> "msg" :> "all"
          :> Get '[JSON] [Message]
      :<|> "msg" :> "like"
          :> Capture "id" (Key Message)
          :> Get '[JSON] String

server :: ConnectionPool -> Server API
server pool = newMsg
         :<|> getMsg
         :<|> allMsgs
         :<|> likeMsg

  where getMsg mid = liftIO $ flip runSqlPersistMPool pool $ do
          msg <- selectFirst [MessageId ==. mid] []
          return $ entityVal <$> msg

        newMsg msg = liftIO $ flip runSqlPersistMPool pool $ do
          insert msg

        allMsgs = liftIO $ flip runSqlPersistMPool pool $ do
          msgs <- selectList [] []
          return $ entityVal <$> msgs

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


app :: ConnectionPool -> Application
app pool = serve api (server pool)


mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool


startApp :: IO ()
startApp = mkApp "sqlite.db" >>= run 3000
