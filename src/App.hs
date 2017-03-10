{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module App where

import Servant
import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Data.String.Conversions
import Control.Monad.Logger (runStderrLoggingT)

import qualified UserApi as U
import qualified MessageApi as M
import qualified StaticApi as S
import qualified ReaderServer as R
import qualified StateServer as STA


import Models

type API = "users"  :> U.API
      :<|> "static" :> S.API
      :<|> "msg"    :> M.API
      :<|> "r"      :> R.API
      :<|> "s"      :> STA.API

server :: ConnectionPool -> Server API
server cp = U.server
       :<|> S.server
       :<|> M.server cp
       :<|> R.server
       :<|> STA.server

app :: FilePath -> IO Application
app dbPath = do
  pool <- getConnectionPool dbPath
  return $ serve api (server pool)

getConnectionPool :: FilePath -> IO ConnectionPool
getConnectionPool sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return pool


api :: Proxy API
api = Proxy

runApp :: IO ()
runApp = run 8080 =<< app "sqlite.db"
