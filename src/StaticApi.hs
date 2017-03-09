{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module StaticApi (server, API) where

import Servant

type API = Raw

server :: Server API
server = serveDirectory "./static"
