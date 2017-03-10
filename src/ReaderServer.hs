{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RankNTypes      #-}


module ReaderServer (server, API) where

import Servant

import Control.Monad.Reader

type API = "a" :> Get '[JSON] String
      :<|> "b" :> Get '[JSON] Int

readerServerT :: ServerT API (Reader String)
readerServerT = a :<|> b
  where a :: Reader String String
        a = return "a"
        b :: Reader String Int
        b = return 228

readerToHandler :: Reader String :~> Handler
readerToHandler = Nat readerToHandler'
  where readerToHandler' :: forall a. Reader String a -> Handler a
        readerToHandler' r = return (runReader r "hi")

server :: Server API
server = enter readerToHandler readerServerT
