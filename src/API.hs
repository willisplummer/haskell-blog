{-# LANGUAGE DataKinds, DeriveGeneric, TypeFamilies, TypeOperators, OverloadedStrings #-}

module API where

import           Control.Concurrent             ( forkIO )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Except     ( throwE )
import           Control.Monad.Reader
import           Crypto.BCrypt                  ( validatePassword )
import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , object
                                                , (.=)
                                                , FromJSON
                                                , parseJSON
                                                , (.:)
                                                , withObject
                                                , Object
                                                )
import           Data.Aeson.Types               ( Parser
                                                , Pair
                                                )
import qualified Data.ByteString.Char8          as BS ( ByteString )
import qualified Data.ByteString.Char8          as B8 ( pack, unpack )
import           Data.Int                       ( Int64 )
import           Data.Maybe                     ( fromJust )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.Persist               ( Key
                                                , Entity
                                                )
import           Database.Persist.Sql           ( SqlBackend )
import           Database.Persist.Postgresql    ( ConnectionString(..)
                                                , fromSqlKey
                                                )
import           GHC.Generics
import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , run
                                                , runSettings
                                                , setBeforeMainLoop
                                                , setPort
                                                )
import           Servant
import           Servant.API
import           Servant.Auth
import           Servant.Auth.Server            ( Auth
                                                , AuthResult(..)
                                                , BasicAuthCfg
                                                , CookieSettings
                                                , JWTSettings
                                                , SetCookie
                                                , FromBasicAuthData
                                                , FromJWT
                                                , ToJWT
                                                , acceptLogin
                                                , fromBasicAuthData
                                                , generateKey
                                                , defaultJWTSettings
                                                , defaultCookieSettings
                                                , makeJWT
                                                , throwAll
                                                )
import           System.IO

import           Schema (NewUser(..), PresentationalUser(..), User, Post, presentationalizeUser, userHashedPassword)
import           Database (createGetUserPG, fetchUserByEmailPG, fetchUsersPG, fetchPostsPG)

instance ToJWT PresentationalUser
instance FromJWT PresentationalUser

data Login = Login { email :: BS.ByteString, password :: BS.ByteString }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

-- Here is the login handler
checkCreds
  :: CookieSettings
  -> JWTSettings
  -> ConnectionString
  -> Login
  -> Handler
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           NoContent
       )
checkCreds cookieSettings jwtSettings connString (Login email password) = do
  mUser <- liftIO $ fetchUserByEmailPG connString email
  mApplyCookies <- if validatePassword (userHashedPassword $ fromJust mUser) password
    then liftIO $ acceptLogin cookieSettings jwtSettings $ presentationalizeUser $ fromJust mUser
    else throwError err401
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> return $ applyCookies NoContent

-- Here is the signup handler
createNewUser
  :: CookieSettings
  -> JWTSettings
  -> ConnectionString
  -> NewUser
  -> Handler
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           NoContent
       )
createNewUser cookieSettings jwtSettings connString newUser =
  do
    mUser <- liftIO $ createGetUserPG connString newUser
    mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings $ presentationalizeUser $ fromJust mUser
    case mApplyCookies of
      Nothing           -> throwError err401
      Just applyCookies -> return $ applyCookies NoContent

fetchUsersHandler :: ConnectionString -> Handler [User]
fetchUsersHandler connString = liftIO $ fetchUsersPG connString

fetchPostsHandler :: ConnectionString -> BS.ByteString -> Handler [Schema.Post]
fetchPostsHandler connString email = liftIO $ fetchPostsPG connString email

type Protected
   = "name" :> Get '[JSON] BS.ByteString
 :<|> "email" :> Get '[JSON] BS.ByteString
 :<|> "users" :> Get '[JSON] [User]
 :<|> "posts" :> Get '[JSON] [Schema.Post]

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: ConnectionString -> AuthResult PresentationalUser -> Server Protected
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected connString (Servant.Auth.Server.Authenticated (PUser name email)) =
  return name :<|> return email :<|> fetchUsersHandler connString :<|> fetchPostsHandler connString email 
-- Otherwise, we return a 401.
protected _ _ = throwAll err401

type Unprotected =
 ("login"
     :> ReqBody '[JSON] Login
     :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
                                       NoContent))
  :<|>
  ("signup"
    :> ReqBody '[JSON] NewUser
    :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
                                        NoContent))
  :<|> Raw

unprotected :: CookieSettings -> JWTSettings -> ConnectionString -> Server Unprotected
unprotected cs jwts connString =
  checkCreds cs jwts connString :<|> createNewUser cs jwts connString :<|> serveDirectoryFileServer
    "static"

type API auths = (Servant.Auth.Server.Auth auths PresentationalUser :> Protected) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> ConnectionString -> Server (API auths)
server cs jwts connString = protected connString :<|> unprotected cs jwts connString

mkApp :: ConnectionString -> IO Application
mkApp connString = do
  -- We generate the key for signing tokens. This would generally be persisted,
  -- and kept safely
  myKey <- generateKey
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtCfg = defaultJWTSettings myKey
      cfg    = defaultCookieSettings :. jwtCfg :. EmptyContext
      --- Here we actually make concrete
      api    = Proxy :: Proxy (API '[JWT])
  pure $ serveWithContext api cfg (server defaultCookieSettings jwtCfg connString)


runServer :: Int -> ConnectionString -> IO ()
runServer port connString = do
  putStrLn "connString:" 
  putStrLn $ B8.unpack connString
  let settings =
        setPort port
          $ setBeforeMainLoop
              (hPutStrLn stderr ("listening on port " ++ show port))
          $ defaultSettings
  runSettings settings =<< mkApp connString
