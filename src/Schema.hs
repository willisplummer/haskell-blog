{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Schema where

import           Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject
                            , Object)
import           Data.Aeson.Types (Parser, Pair)
import           Database.Persist (Entity(..), Entity)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)
import qualified Data.ByteString.Char8(ByteString, unpack)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    hashedPassword Data.ByteString.Char8.ByteString
    UniqueEmail email
    deriving Show Read
|]

instance ToJSON User where
  toJSON user = object 
    [ "name" .= userName user
    , "email" .= userEmail user
    -- dont send pwHash to frontend
    -- , "hashedPassword" .= Data.ByteString.Char8.unpack (userHashedPassword user)
    ]

data RawUser = RawUser {
    ruName :: Text
  , ruEmail :: Text
  , ruPassword :: Text
} deriving (Show, Read)

instance FromJSON RawUser where
  parseJSON = withObject "User" parseRawUser

parseRawUser :: Object -> Parser RawUser
parseRawUser o = do
  name <- o .: "name"
  email <- o .: "email"
  password <- o .: "password"
  return RawUser
    { ruName = name
    , ruEmail = email
    , ruPassword = password
    }
