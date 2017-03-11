{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}


module UserApi (server, API) where

import Data.Aeson
import Data.Aeson.TH
import Servant
import Servant.Docs hiding (API)
import Data.ByteString.Lazy.Char8 as BS (pack)
import Data.List

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

newtype APIdocs = APIdocs String
instance MimeRender PlainText APIdocs where
   mimeRender _ (APIdocs d) = BS.pack d

newtype HasUserResponce = HasUserResponce String
instance MimeRender PlainText HasUserResponce where
   mimeRender _ (HasUserResponce r) = BS.pack r

type API = Get '[JSON] [User]
      :<|> "user"       :> Capture "id" Int         :> Get '[JSON] (Maybe User)
      :<|> "has"        :> QueryParam "name" String :> Get '[PlainText] HasUserResponce
      :<|> "logicians"  :> Get '[JSON] [User]
      :<|> "physicists" :> Get '[JSON] [User]
      :<|> "docs"       :> Get '[PlainText] APIdocs


api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> return . findUserById
    :<|> return . findUserByName
    :<|> return logicians
    :<|> return physicists
    :<|> return documentation

  where findUserById uid = find ((== uid) . userId) users

        findUserByName :: Maybe String -> HasUserResponce
        findUserByName maybeName = case maybeName of
          Just n  -> case find (\(User _ f s) -> f == n || s == n) users of
            Just (User i f s) -> HasUserResponce $
              "We have user " ++ f ++ " " ++ s ++ " (" ++ show i ++ ") in our DB"
            Nothing -> HasUserResponce $ "Cant find user " ++ n ++ " in our DB"
          Nothing -> HasUserResponce $ "Username not specified"

        documentation :: APIdocs
        documentation = APIdocs $ markdown $ docs api

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

instance ToCapture (Capture "id" Int) where
 toCapture _ =
   DocCapture "id"
              "(integer) user id"

instance ToParam (QueryParam "name" String) where
  toParam _ =
    DocQueryParam "name"
                  ["Gotlob", "Frege", "..."]
                  "First or second User name"
                  Normal -- { Normal | List | Flag }

instance ToSample User where
  toSamples _ =
    [ ("Example of one user", User 10 "Per" "Martin-Lof")
    , ("Example of another user", User 20 "Benjamin" "Pierce")
    ]

instance ToSample APIdocs where
  toSamples _ =
    [ ("Exq", APIdocs $ "long md file")

    ]

instance ToSample HasUserResponce where
  toSamples _ =
    [ ("Success", HasUserResponce $ "We have user Steve Awodey (10) in our DB")
    , ("Fail 1", HasUserResponce $ "Cant find user MacLane in our DB")
    , ("Fail 2", HasUserResponce $ "Username not specified")
    ]
