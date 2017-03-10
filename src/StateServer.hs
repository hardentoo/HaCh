{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RankNTypes      #-}


module StateServer (server, API) where

import Servant

import Control.Monad.State

type API = "a" :> Capture "x" Int :> Get '[JSON] Int
      :<|> "m" :> Capture "x" Int :> Get '[JSON] Int

stateServerT :: ServerT API (State Int)
stateServerT = a :<|> b
  where a :: Int -> State Int Int
        a x = (+x) <$> get >>= return
        b :: Int -> State Int Int
        b x = (*x) <$> get >>= return

stateToHandler :: State Int :~> Handler
stateToHandler = Nat stateToHandler'
  where stateToHandler' :: forall a. State Int a -> Handler a
        stateToHandler' s = return $ fst $ runState s 20

server :: Server API
server = enter stateToHandler stateServerT
