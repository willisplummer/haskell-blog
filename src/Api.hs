{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Control.Monad.Reader
import           Crypto.BCrypt(validatePassword)
import qualified Data.ByteString.Char8(ByteString, pack)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server

import           Database (fetchUserByEmailPG, fetchUserPG, createUserPG, fetchPostgresConnection)
import           Schema

type UsersAPI = 
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] RawUser :> Post '[JSON] Int64
  :<|> "authenticate" :> ReqBody '[JSON] UserAuth :> Post '[JSON] Bool

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fetchUsersHandler :: ConnectionString -> Int64 -> Handler User
fetchUsersHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

validatePassword' :: Data.ByteString.Char8.ByteString -> Text -> Bool
validatePassword' hashedPw testPw =
  Crypto.BCrypt.validatePassword hashedPw (Data.Text.Encoding.encodeUtf8 testPw)

authenticateUserHandler :: ConnectionString -> UserAuth -> Handler Bool
authenticateUserHandler connString userAuth = do
  maybeUser <- liftIO $ fetchUserByEmailPG connString (authEmail userAuth)
  case maybeUser of
    Just user -> return (validatePassword' (userHashedPassword user) (authPassword userAuth))
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
