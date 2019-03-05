{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
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

instance FromJSON BS.ByteString where
  parseJSON src = do
    str <- parseJSON src
    return $ B8.pack str

instance ToJSON BS.ByteString where
  toJSON = toJSON . B8.unpack

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name BS.ByteString
    email BS.ByteString
    hashedPassword BS.ByteString
    UniqueEmail email
    deriving Show Read
  Follow sql=follows
    followerId UserId
    followedId UserId
    deriving Show Read
  Judgeable sql=judgeables
    name BS.ByteString
    imageUrl BS.ByteString
    deriving Show Read
  Judgement sql=follows
    judgeableId JudgeableId
    userId UserId
    isGood Bool
    deriving Show Read
|]

instance ToJSON Judgeable where
  toJSON = toJSON
instance FromJSON Judgeable where
  parseJSON = parseJSON

instance ToJSON (Entity Judgeable) where
  toJSON = toJSON
instance FromJSON (Entity Judgeable) where
  parseJSON = parseJSON

instance ToJSON Judgement where
  toJSON = toJSON
instance FromJSON Judgement where
  parseJSON = parseJSON

instance ToJSON (Entity Judgement) where
  toJSON = toJSON
instance FromJSON (Entity Judgement) where
  parseJSON = parseJSON

data PresentationalUser = PUser {
  puName :: BS.ByteString,
  puEmail :: BS.ByteString,
  pId :: Key User
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

