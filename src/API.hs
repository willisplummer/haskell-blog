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
import qualified Data.ByteString.Char8         as BS
                                                ( ByteString )
import qualified Data.ByteString.Char8         as B8
                                                ( pack
                                                , unpack
                                                )
import           Data.Int                       ( Int64 )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.Persist               ( Key
                                                , Entity
                                                , entityVal
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

import           Schema                         ( NewUser(..)
                                                , PresentationalUser(..)
                                                , User
                                                , presentationalizeUser
                                                , userHashedPassword
                                                )
import           Database                       ( createGetUserPG
                                                , fetchUserByEmailPG
                                                , fetchUserPG
                                                , fetchUsersPG
                                                , fetchPostsPG
                                                )

instance ToJWT PresentationalUser
instance FromJWT PresentationalUser

data Login = Login { email :: BS.ByteString, password :: BS.ByteString }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

data Judgeable = Judgeable { id :: Int64, name :: BS.ByteString, imageUrl :: BS.ByteString }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Judgeable
instance FromJSON Judgeable

data Judgement = Judgement { judgeableId :: Int64, userId :: Key User, isGood :: Bool }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Judgement
instance FromJSON Judgement

type JudgeablesAPI = 
  "judgeables" :> Get '[JSON] [Judgeable]
  :<|> "judgeables" :> Capture "id" Int64 :> Get '[JSON] Judgeable
  :<|> "judgeables" :> ReqBody '[JSON] Judgeable :> Post '[JSON] Judgeable
  :<|> "judgeables" :> Capture "id" Int64 :> "judgements" :> ReqBody '[JSON] Bool :> Post '[JSON] Judgement

judgeablesServer :: PresentationalUser -> Server JudgeablesAPI
judgeablesServer currentUser =
  return [banana]
  :<|> (\_id -> return banana)
  :<|> (\newJudgeable -> return newJudgeable)
  :<|> (\judgeableId isGood -> return $ Judgement judgeableId (pId currentUser) isGood)
  where
    banana = Judgeable 1 "banana" "url goes here"   

type UsersAPI =
  "users" :> Get '[JSON] [Entity User]
  :<|> "users" :> Capture "id" Int64 :> Get '[JSON] (Entity User)
  :<|> "users" :> Capture "id" Int64 :> "subscribe" :> Servant.API.PostNoContent '[JSON] NoContent

usersServer :: ConnectionString -> PresentationalUser -> Server UsersAPI
usersServer connString currentUser =
  getUsersHandler connString
  :<|> getUserHandler connString
  :<|> subscribeToUserHandler connString currentUser
  where
    getUsersHandler :: ConnectionString -> Handler [Entity User]
    getUsersHandler connString = liftIO $ fetchUsersPG connString

    getUserHandler :: ConnectionString -> Int64 -> Handler (Entity User)
    getUserHandler connString id = do
      mUser <- liftIO $ fetchUserPG connString id
      case mUser of
        Nothing           -> throwError err404
        Just user -> return user

    -- TODO: Actually handle the request
    subscribeToUserHandler :: ConnectionString -> PresentationalUser -> Int64 -> Handler NoContent 
    subscribeToUserHandler connString currentUser subscribeToId = return NoContent

type Protected =
  JudgeablesAPI
  :<|> UsersAPI

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: ConnectionString -> AuthResult PresentationalUser -> Server Protected
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected connString (Authenticated currentUser) =
  judgeablesServer currentUser
  :<|> usersServer connString currentUser
-- Otherwise, we return a 401.
protected _ _ = throwAll err401

loginHandler
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

loginHandler cookieSettings jwtSettings connString (Login email password) = do
  mUser         <- liftIO $ fetchUserByEmailPG connString email
  case mUser of
    Nothing -> throwError err401
    Just user -> do
      mApplyCookies <-
        if validatePassword (userHashedPassword $ entityVal user) password
          then
            liftIO
            $ acceptLogin cookieSettings jwtSettings
            $ presentationalizeUser user
          else throwError err401
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent

signupHandler
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
signupHandler cookieSettings jwtSettings connString newUser = do
  mUser         <- liftIO $ createGetUserPG connString newUser
  case mUser of
    Nothing -> throwError err401
    Just user -> do
      mApplyCookies <-
        liftIO
        $ acceptLogin cookieSettings jwtSettings
        $ presentationalizeUser user
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent

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
unprotected
  :: CookieSettings -> JWTSettings -> ConnectionString -> Server Unprotected
unprotected cs jwts connString =
  loginHandler cs jwts connString
    :<|> signupHandler cs jwts connString

type API auths = (Servant.Auth.Server.Auth auths PresentationalUser :> Protected) :<|> Unprotected

server
  :: CookieSettings -> JWTSettings -> ConnectionString -> Server (API auths)
server cs jwts connString =
  protected connString :<|> unprotected cs jwts connString

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
  pure $ serveWithContext api
                          cfg
                          (server defaultCookieSettings jwtCfg connString)


runServer :: Int -> ConnectionString -> IO ()
runServer port connString = do
  putStrLn "connString:"
  putStrLn $ B8.unpack connString
  let settings =
        setPort port
          $ setBeforeMainLoop
            (hPutStrLn stderr ("listening on port " ++ show port))
            defaultSettings
  runSettings settings =<< mkApp connString
