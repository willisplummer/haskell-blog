module Main where

  import API (runServer)
  import Database (localConnString, migrateDB)

  main :: IO ()
  main = do
    migrated <- migrateDB localConnString
    runServer
