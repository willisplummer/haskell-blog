{-# LANGUAGE DeriveGeneric              #-}

module Data.NewUser (NewUser(..)) where

import           Data.Aeson
import           Data.Char
import           GHC.Generics
import           Schema

data NewUser = NewUser {
  nuUserName :: String
  , nuEmail :: String
  , nuPassword :: String
} deriving (Eq, Show, Read, Generic)

instance FromJSON NewUser where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 2
  }
