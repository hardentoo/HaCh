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

import Servant

import Control.Monad.IO.Class  (liftIO)
import Database.Persist.Sqlite

import Models



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
