{-# LANGUAGE DataKinds, DeriveGeneric, TypeFamilies, TypeOperators, OverloadedStrings #-}

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
import           Data.Maybe(fromJust)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Database.Persist (Key, Entity)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Postgresql (ConnectionString(..), fromSqlKey)
import           GHC.Generics
import           Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import           Servant
import           Servant.API
import           Servant.Auth
import           Servant.Auth.Server (AuthResult(..), BasicAuthCfg, FromBasicAuthData, FromJWT, ToJWT, fromBasicAuthData, generateKey, defaultJWTSettings, defaultCookieSettings, throwAll)
import           System.IO

import           Database (fetchUserByEmailPG, fetchUserByEmailViaConnectionPG, fetchUserPG, createUserPG, fetchPostgresConnection)
import           Schema

port :: Int
port = 3001

-- todo: dont auth the authenticate route?
type UsersAPI = 
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] RawUser :> Post '[JSON] Int64
  :<|> "authenticate" :> ReqBody '[JSON] BasicAuthData :> Post '[JSON] AuthenticatedUser

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

fetchUsersHandler :: ConnectionString -> Int64 -> Handler User
fetchUsersHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

validatePassword' :: Data.ByteString.Char8.ByteString -> Data.ByteString.Char8.ByteString -> Bool
validatePassword' hashedPw testPw =
  Crypto.BCrypt.validatePassword hashedPw testPw

data AuthenticatedUser = AUser {
    auID :: Int64
  , auOrgID :: Int
} deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

authenticateUserHandler :: ConnectionString -> BasicAuthData -> Handler AuthenticatedUser
authenticateUserHandler connString (BasicAuthData email password) = do
  maybeUser <- liftIO $ fetchUserByEmailPG connString email
  return (fromJust (authUser password maybeUser))

authUser :: Data.ByteString.Char8.ByteString -> Maybe User -> Maybe AuthenticatedUser
authUser pw mbU =
  case mbU of
    Just u ->
      if (validatePassword' (userHashedPassword u) pw)
        then Just (AUser {
          auID = 1,
          auOrgID = 1
        })
        else Nothing
    Nothing -> Nothing

authCheck :: ConnectionString
  -> BasicAuthData
  -> IO (AuthResult AuthenticatedUser)
authCheck connString (BasicAuthData login password) = do
  mbUser <- fetchUserByEmailPG connString login
  return (maybe Indefinite Authenticated $ (authUser password mbUser))

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

createUserHandler :: ConnectionString -> RawUser -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user

type UsersApiServer = Auth '[JWT] AuthenticatedUser :> UsersAPI

server :: ConnectionString -> Server UsersApiServer
server connString (Authenticated user) =
  (fetchUsersHandler connString) :<|> 
  (createUserHandler connString) :<|>
  (authenticateUserHandler connString)
server connString _ = throwAll err401


mkApp :: ConnectionString -> IO Application
mkApp connString = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck connString
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy UsersApiServer
      s = server connString
  pure $ serveWithContext api cfg s

runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  let settings = setPort port $ setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $ defaultSettings
  runSettings settings =<< mkApp connString
  
