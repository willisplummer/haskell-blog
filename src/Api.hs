{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Control.Monad.Reader
import           Crypto.BCrypt(validatePassword)
import           Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject
                            , Object)
import           Data.Aeson.Types (Parser, Pair)
import qualified Data.ByteString.Char8(ByteString, pack)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Auth
import           Servant.Server

import           Database (fetchUserByEmailPG, fetchUserPG, createUserPG, fetchPostgresConnection)
import           Schema

type UsersAPI = 
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] RawUser :> Post '[JSON] Int64
  :<|> "authenticate" :> ReqBody '[JSON] BasicAuthData :> Post '[JSON] Bool

parseUserAuth :: Object -> Parser BasicAuthData
parseUserAuth o = do
  email <- o .: "email"
  password <- o .: "password"
  return BasicAuthData
    { basicAuthUsername = Data.ByteString.Char8.pack email
    , basicAuthPassword = Data.ByteString.Char8.pack password
    }

instance FromJSON BasicAuthData where
  parseJSON = withObject "BasicAuthData" parseUserAuth
  
usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fetchUsersHandler :: ConnectionString -> Int64 -> Handler User
fetchUsersHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

validatePassword' :: Data.ByteString.Char8.ByteString -> Data.ByteString.Char8.ByteString -> Bool
validatePassword' hashedPw testPw =
  Crypto.BCrypt.validatePassword hashedPw testPw

authenticateUserHandler :: ConnectionString -> BasicAuthData -> Handler Bool
authenticateUserHandler connString (BasicAuthData email password) = do
  maybeUser <- liftIO $ fetchUserByEmailPG connString email
  case maybeUser of
    Just user -> return (validatePassword' (userHashedPassword user) password)
    Nothing -> return False

createUserHandler :: ConnectionString -> RawUser -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user

usersServer :: ConnectionString -> Server UsersAPI
usersServer connString = 
  (fetchUsersHandler connString) :<|> 
  (createUserHandler connString) :<|>
  (authenticateUserHandler connString)

runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  run 8000 (serve usersAPI (usersServer connString))
