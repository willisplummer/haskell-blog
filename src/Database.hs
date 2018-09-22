{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Crypto.BCrypt(validatePassword, hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.Int (Int64)
import qualified Data.ByteString.Char8(ByteString)
import           Data.Maybe(fromJust)
import           Data.Pool(Pool)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Database.Persist (entityVal, selectFirst, get, insert, delete, selectList, (==.))
import           Database.Persist.Sql (fromSqlKey, toSqlKey, SqlBackend)
import           Database.Persist.Postgresql (Connection, ConnectionString, withPostgresqlConn, runMigration, runMigrationUnsafe, SqlPersistT)

import           Schema

localConnString :: ConnectionString
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres"

-- This is IO since in a real application we'd want to configure it.
fetchPostgresConnection :: IO ConnectionString
fetchPostgresConnection = return localConnString

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

-- TODO: replace runMigrationUnsafe before deploying
migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigrationUnsafe migrateAll)

fetchUsersPG :: ConnectionString -> IO [User]
fetchUsersPG connString = do
  entities <- runAction connString (selectList [] [])
  return (fmap entityVal entities)

fetchUserPG :: ConnectionString -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

fetchUserByEmailPG :: ConnectionString -> Data.ByteString.Char8.ByteString -> IO (Maybe User)
fetchUserByEmailPG connString email = do
  entity <- runAction connString (selectFirst [UserEmail ==. (decodeUtf8 email)] [])
  return (fmap entityVal entity)

fetchUserByEmailViaConnectionPG :: SqlBackend -> Data.ByteString.Char8.ByteString -> IO (Maybe User)
fetchUserByEmailViaConnectionPG connection email = do
  entity <- runReaderT (selectFirst [UserEmail ==. (decodeUtf8 email)] []) (connection :: SqlBackend)
  return (fmap entityVal entity)

hashPassword :: Data.Text.Text -> IO (Data.ByteString.Char8.ByteString)
hashPassword t = do
  mb <- Crypto.BCrypt.hashPasswordUsingPolicy Crypto.BCrypt.slowerBcryptHashingPolicy (Data.Text.Encoding.encodeUtf8 t)
  return (fromJust mb)

hashUser :: RawUser -> IO User
hashUser rawUser = do
  hashedPW <- hashPassword (ruPassword rawUser )
  return User {
    userName = ruName rawUser,
    userEmail = ruEmail rawUser,
    userHashedPassword = hashedPW
  }

createUserPG :: ConnectionString -> RawUser -> IO Int64
createUserPG connString rawUser = do
  hashedUser <- hashUser rawUser
  insertedUser <- runAction connString (insert hashedUser)
  return (fromSqlKey insertedUser)

deleteUserPG :: ConnectionString -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid
