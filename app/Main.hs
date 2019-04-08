{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           System.Environment
import           Database.Persist.Postgresql    ( PostgresConf(..) )

import           API                            ( mainWithCookies )
import           Database                       ( migrateDB )
import           Helpers.DatabaseURL            ( fromDatabaseUrl )

main :: IO ()
main = mainWithCookies
