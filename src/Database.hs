{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ExistentialQuantification #-}

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
import qualified Data.ByteString.Char8         as B8

import           Data.NewUser
import           Data.Pool                      ( Pool )

import           Database.Persist               ( entityKey
                                                , entityVal
                                                , Entity(..)
                                                , selectFirst
                                                , get
                                                , insert
                                                , delete
                                                , selectList
                                                , (==.)
                                                )
import Database.Persist.Class (ToBackendKey)
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

fetchUsersPG :: ConnectionString -> IO [Entity User]
fetchUsersPG connString = runAction connString (selectList [] [])

fetchUserPG :: ConnectionString -> Int64 -> IO (Maybe (Entity User))
fetchUserPG connString uid = do
  mUser <- runAction connString (get userKey)
  return $ Entity userKey <$> mUser
  where
    userKey :: Key User
    userKey = toSqlKey uid

fetchUserByEmailPG :: ConnectionString -> String -> IO (Maybe (Entity User))
fetchUserByEmailPG connString email = runAction connString (selectFirst [UserEmail ==. email] [])

fetchUserByEmailViaConnectionPG
  :: SqlBackend -> String -> IO (Maybe User)
fetchUserByEmailViaConnectionPG connection email = do
  entity <- runReaderT (selectFirst [UserEmail ==. email] [])
                       (connection :: SqlBackend)
  return (fmap entityVal entity)

hashPassword :: BS.ByteString -> IO (Maybe BS.ByteString)
hashPassword =
  Crypto.BCrypt.hashPasswordUsingPolicy Crypto.BCrypt.slowerBcryptHashingPolicy

  -- instance FromJSON BS.ByteString where
  --   parseJSON src = do
  --     str <- parseJSON src
  --     return $ B8.pack str
  
  -- instance ToJSON BS.ByteString where
  --   toJSON = toJSON . B8.unpack

    
hashUser :: NewUser -> IO (Maybe User)
hashUser (NewUser name email pw) = do
  mHashedPW <- hashPassword $ B8.pack pw
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

createGetUserPG :: ConnectionString -> NewUser -> IO (Maybe (Entity User))
createGetUserPG connString newUser = do
  newUserKeyInt <- createUserPG connString newUser
  fetchedUser   <- sequence $ fetchUserPG connString <$> newUserKeyInt
  return (join fetchedUser)

deleteUserPG :: ConnectionString -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
 where
  userKey :: Key User
  userKey = toSqlKey uid

fetchEntity :: forall a. ToBackendKey SqlBackend a => ConnectionString -> Int64 -> IO (Maybe (Entity a))
fetchEntity connString id = do 
  mEntity <- runAction connString $ get key
  return $ Entity key <$> mEntity
  where
    key = toSqlKey id
  

-- JUDGEABLES

fetchJudgeablesPG :: ConnectionString -> IO [Entity Judgeable]
fetchJudgeablesPG connString = runAction connString (selectList [] [])

fetchJudgeablePG :: ConnectionString -> Int64 -> IO (Maybe (Entity Judgeable))
fetchJudgeablePG = fetchEntity

createJudgeablePG :: ConnectionString -> Judgeable -> IO (Maybe (Entity Judgeable))
createJudgeablePG connString judgeable = do
  key <- runAction connString $ insert judgeable
  fetchJudgeablePG connString $ fromSqlKey key

-- JUDGEMENTS

fetchJudgementPG :: ConnectionString -> Int64 -> IO (Maybe (Entity Judgement))
fetchJudgementPG = fetchEntity

createJudgementPG :: ConnectionString -> Judgement -> IO (Maybe (Entity Judgement))
createJudgementPG connString judgement = do
  key <- runAction connString $ insert judgement
  fetchJudgementPG connString $ fromSqlKey key

-- FOLLOWS

fetchFollowPG' :: ConnectionString -> Int64 -> IO (Maybe (Entity Follow))
fetchFollowPG' = fetchEntity

fetchFollowPG :: ConnectionString -> Int64 -> IO (Maybe (Entity Follow))
fetchFollowPG connString followId = do
  mFollow <- runAction connString $ get followKey
  return (Entity followKey <$> mFollow)
  where
    followKey :: Key Follow
    followKey = toSqlKey followId

createFollowPG :: ConnectionString -> Follow -> IO (Maybe (Entity Follow))
createFollowPG connString follow = do
  key <- runAction connString $ insert follow
  fetchFollowPG connString $ fromSqlKey key
