{-# LANGUAGE ScopedTypeVariables #-}

module MigrateDB where

import           System.Environment
import           Database.Persist.Postgresql    ( PostgresConf(..) )

import           Database                       ( migrateDB )
import           Helpers.DatabaseURL            ( fromDatabaseUrl )

main :: IO ()
main = do
    putStr "DATABASE_URL:" >> (putStrLn =<< getEnv "DATABASE_URL")
    dbUrl                              <- getEnv "DATABASE_URL"
    (port :: Int                     ) <- read <$> getEnv "PORT"
    (PostgresConf connString poolSize) <- fromDatabaseUrl 5 dbUrl

    migrateDB connString
