module Database where

import           Control.Monad.Logger           ( runStdoutLoggingT
                                                , MonadLogger
                                                , LoggingT
                                                )
import           Control.Monad.Reader           ( join
                                                , runReaderT
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Crypto.BCrypt                  ( validatePassword
                                                , hashPasswordUsingPolicy
                                                , slowerBcryptHashingPolicy
                                                )
import           Data.Int                       ( Int64 )
import qualified Data.ByteString               as BS
                                                ( ByteString )
import           Data.Pool                      ( Pool )

import           Database.Persist               ( entityKey
                                                , entityVal
                                                , selectFirst
                                                , get
                                                , insert
                                                , delete
                                                , selectList
                                                , (==.)
                                                )
import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
                                                , SqlBackend
                                                )
import           Database.Persist.Postgresql    ( Connection
                                                , ConnectionString
                                                , PostgresConf(..)
                                                , withPostgresqlConn
                                                , runMigration
                                                , runMigrationUnsafe
                                                , SqlPersistT
                                                )
import           System.Environment

import           Schema

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

fetchPostsPG :: ConnectionString -> BS.ByteString -> IO [Schema.Post]
fetchPostsPG connString email = do
  entity <- runAction connString (selectFirst [UserEmail ==. email] [])
  let userId = entityKey <$> entity
  case userId of
    Just id -> do
      entities <- runAction connString $ selectList [PostUser ==. id] []
      return (fmap entityVal entities)
    Nothing -> return []

fetchUserPG :: ConnectionString -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

fetchUserByEmailPG :: ConnectionString -> BS.ByteString -> IO (Maybe User)
fetchUserByEmailPG connString email = do
  entity <- runAction connString (selectFirst [UserEmail ==. email] [])
  return (fmap entityVal entity)

fetchUserByEmailViaConnectionPG
  :: SqlBackend -> BS.ByteString -> IO (Maybe User)
fetchUserByEmailViaConnectionPG connection email = do
  entity <- runReaderT (selectFirst [UserEmail ==. email] [])
                       (connection :: SqlBackend)
  return (fmap entityVal entity)

hashPassword :: BS.ByteString -> IO (Maybe BS.ByteString)
hashPassword =
  Crypto.BCrypt.hashPasswordUsingPolicy Crypto.BCrypt.slowerBcryptHashingPolicy

hashUser :: NewUser -> IO (Maybe User)
hashUser (NewUser name email pw) = do
  mHashedPW <- hashPassword pw
  return
    $   (\hashedPW -> User
          { userName           = name
          , userEmail          = email
          , userHashedPassword = hashedPW
          }
        )
    <$> mHashedPW

createUserPG :: ConnectionString -> NewUser -> IO (Maybe Int64)
createUserPG connString newUser = do
  mHashedUser  <- hashUser newUser
  insertedUser <- sequence (runAction connString <$> (insert <$> mHashedUser))
  return (fromSqlKey <$> insertedUser)

createGetUserPG :: ConnectionString -> NewUser -> IO (Maybe User)
createGetUserPG connString newUser = do
  newUserKeyInt <- createUserPG connString newUser
  fetchedUser   <- sequence $ fetchUserPG connString <$> newUserKeyInt
  return (join fetchedUser)

deleteUserPG :: ConnectionString -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
 where
  userKey :: Key User
  userKey = toSqlKey uid
