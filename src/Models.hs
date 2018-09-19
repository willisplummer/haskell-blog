{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Text

import Database.Persist.TH as PTH
import Database.Persist.Sql as SQL
import Database.Persist (Entity(..))

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
User sql=users
  name Text
  email Text
  age  Int
  UniqueEmail email
  deriving Read Show
|]

sampleUser :: Entity User
sampleUser = Entity (SQL.toSqlKey 1) $ User
  { userName = "admin"
  , userEmail = "admin@test.com"
  , userAge = 23
  }

instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "name"
         <*> v .: "email"
         <*> v .: "age"

instance ToJSON User where
  toJSON (User name email age) =
    object [ "name" .= name
           , "email"  .= email
           , "age"  .= age ]
