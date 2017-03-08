{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module StaticApi (server, API) where

import Servant
import Servant.Server
import Servant.Utils.StaticFiles

type API = Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "./static"

app :: Application
app = serve api server
