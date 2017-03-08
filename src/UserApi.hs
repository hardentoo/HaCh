{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module UserApi
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Data.List

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users"      :> Get '[JSON] [User]
      :<|> "user"       :> Capture "id" Int         :> Get '[JSON] (Maybe User)
      :<|> "haveUser"   :> QueryParam "name" String :> Get '[PlainText] String
      :<|> "logicians"  :> Get '[JSON] [User]
      :<|> "physicists" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> return . findUserById
    :<|> return . findUserByName
    :<|> return logicians
    :<|> return physicists

  where findUserById uid = find ((== uid) . userId) users
        findUserByName maybeName = case maybeName of
          Just n  -> case find (\(User _ f s) -> f == n || s == n) users of
            Just (User i f s) ->
              "We have user " ++ f ++ " " ++ s ++ " (" ++ show i ++ ") in our DB"
            Nothing -> "Cant find user " ++ n ++ " in our DB"
          Nothing -> "Username not specified"

users :: [User]
users = physicists ++ logicians

logicians :: [User]
logicians = [ User 3 "Bertrand" "Russell"
            , User 4 "Gottlob" "Frege"
            ]

physicists :: [User]
physicists = [ User 1 "Isaac" "Newton"
             , User 2 "Albert" "Einstein"
             ]
