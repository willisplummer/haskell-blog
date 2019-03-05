{-# LANGUAGE DeriveGeneric              #-}

module Data.NewUser (NewUser(..)) where

import           Data.Aeson
import           Data.Char
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as B8
import           GHC.Generics
import           Schema

data NewUser = NewUser {
  nuUserName :: BS.ByteString
  , nuEmail :: BS.ByteString
  , nuPassword :: BS.ByteString
} deriving (Eq, Show, Read, Generic)

instance FromJSON NewUser where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 2
  }
