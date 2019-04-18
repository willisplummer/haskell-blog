{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Schema where

import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , object
                                                , (.=)
                                                , FromJSON
                                                , parseJSON
                                                , (.:)
                                                , withObject
                                                , Object
                                                , Value(..)
                                                , defaultOptions
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Aeson.Types               ( Parser
                                                , Pair
                                                )
import           Data.Char                      ( toLower )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..)
                                                , Entity
                                                , Key(..)
                                                )
import qualified Database.Persist.TH           as PTH
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as B8
import           GHC.Generics


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User
    name String
    email String
    hashedPassword BS.ByteString
    UniqueEmail email
    deriving Show Read
  Follow json
    followerId UserId
    followedId UserId
    deriving Show Read
  Judgeable json
    name String
    imageUrl String
    deriving Show Read
  Judgement json
    judgeableId JudgeableId
    userId UserId
    isGood Bool
    deriving Show Read
|]

data PresentationalUser = PUser {
  puName :: String,
  puEmail :: String,
  puId :: Key User
} deriving (Eq, Show, Read, Generic)

instance ToJSON PresentationalUser where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 2
  }

instance FromJSON PresentationalUser where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 2
  }

presentationalizeUser :: Entity User -> PresentationalUser
presentationalizeUser (Entity id (User name email pw)) = PUser name email id

instance ToJSON (Entity User) where
  toJSON = toJSON . presentationalizeUser

