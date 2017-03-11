{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}


module Models where

import Database.Persist
import Database.Persist.TH
import Data.Aeson
import Data.Aeson.TH
import Data.String.Conversions

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message
    heading String Maybe
    text String
    likes Int
    deriving Eq Show
|]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \ v -> do
    messageHeading  <- v .: "heading"
    messageText     <- v .: "text"
    messageLikes    <- pure 0
    return $ Message{..}

instance ToJSON (Entity Message) where
  toJSON (Entity mid (Message heading text likes)) =
    object [
        "id"      .= mid
      , "heading" .= heading
      , "text"    .= text
      , "likes"   .= likes
      ]

instance ToJSON Message where
  toJSON (Message heading text likes) =
    object [
        "heading" .= heading
      , "text"    .= text
      , "likes"   .= likes
      ]
