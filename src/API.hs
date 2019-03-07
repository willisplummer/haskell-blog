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
import           Data.NewUser
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.Persist               ( Key
                                                , Entity
                                                , entityVal
                                                )
import           Database.Persist.Sql           ( SqlBackend, toSqlKey )
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
import           Network.Wai.Middleware.Cors    ( cors
                                                , simpleCorsResourcePolicy
                                                , corsOrigins
                                                , corsRequestHeaders
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

import           Schema                         ( PresentationalUser(..)
                                                , User
                                                , Judgeable
                                                , Judgeable(..)
                                                , Judgement
                                                , Judgement(..)
                                                , Follow
                                                , Follow(..)
                                                , presentationalizeUser
                                                , userHashedPassword
                                                )
import           Database                       ( createGetUserPG
                                                , createJudgeablePG
                                                , createJudgementPG
                                                , createFollowPG
                                                , fetchUserByEmailPG
                                                , fetchUserPG
                                                , fetchUsersPG
                                                , fetchJudgeablesPG
                                                , fetchJudgeablePG
                                                )

instance ToJWT PresentationalUser
instance FromJWT PresentationalUser

data Login = Login { email :: BS.ByteString, password :: BS.ByteString }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

type JudgeablesAPI = 
  "judgeables" :> Get '[JSON] [Entity Judgeable]
  :<|> "judgeables" :> Capture "id" Int64 :> Get '[JSON] (Entity Judgeable)
  :<|> "judgeables" :> ReqBody '[JSON] Judgeable :> Post '[JSON] (Entity Judgeable)
  :<|> "judgeables" :> Capture "id" Int64 :> "judgements" :> ReqBody '[JSON] Bool :> Post '[JSON] (Entity Judgement)

judgeablesServer :: ConnectionString -> PresentationalUser -> Server JudgeablesAPI
judgeablesServer connString currentUser =
  (getJudgeablesHandler connString)
  :<|> (getJudgeableHandler connString)
  :<|> (createJudgeableHandler connString)
  :<|> (createJudgementHandler connString currentUser)
  where
    getJudgeablesHandler :: ConnectionString -> Handler [Entity Judgeable]
    getJudgeablesHandler connString = liftIO $ fetchJudgeablesPG connString

    getJudgeableHandler :: ConnectionString -> Int64 -> Handler (Entity Judgeable)
    getJudgeableHandler connString judgeableId = do
      mJudgeable <- liftIO $ fetchJudgeablePG connString judgeableId
      case mJudgeable of
        Nothing -> throwError err404
        Just judgeable -> return judgeable

    createJudgeableHandler :: ConnectionString -> Judgeable -> Handler (Entity Judgeable)
    createJudgeableHandler connString judgeable = do
      mNewJudgeable <- liftIO $ createJudgeablePG connString judgeable
      case mNewJudgeable of
        Nothing -> throwError err422
        Just newJudgeable -> return newJudgeable

    createJudgementHandler :: ConnectionString -> PresentationalUser -> Int64 -> Bool -> Handler (Entity Judgement)
    createJudgementHandler connString currentUser judgeableId isGood = do
      mNewJudgement <- liftIO $ createJudgementPG connString judgement
      case mNewJudgement of
        Nothing -> throwError err422
        Just newJudgement -> return newJudgement
        where
          judgeableKey :: Key Judgeable
          judgeableKey = toSqlKey judgeableId

          judgement :: Judgement
          judgement = Judgement judgeableKey (pId currentUser) isGood

type UsersAPI =
  "users" :> Get '[JSON] [Entity User]
  :<|> "users" :> Capture "id" Int64 :> Get '[JSON] (Entity User)
  :<|> "users" :> Capture "id" Int64 :> "follow" :> Servant.API.PostNoContent '[JSON] (Entity Follow)

usersServer :: ConnectionString -> PresentationalUser -> Server UsersAPI
usersServer connString currentUser =
  getUsersHandler connString
  :<|> getUserHandler connString
  :<|> followUserHandler connString currentUser
  where
    getUsersHandler :: ConnectionString -> Handler [Entity User]
    getUsersHandler connString = liftIO $ fetchUsersPG connString

    getUserHandler :: ConnectionString -> Int64 -> Handler (Entity User)
    getUserHandler connString id = do
      mUser <- liftIO $ fetchUserPG connString id
      case mUser of
        Nothing           -> throwError err404
        Just user -> return user

    followUserHandler :: ConnectionString -> PresentationalUser -> Int64 -> Handler (Entity Follow) 
    followUserHandler connString user followedId = do
      mFollow <- liftIO $ createFollowPG connString follow
      case mFollow of
        Nothing -> throwError err422
        Just user -> return user
        where
          followedKey :: Key User
          followedKey = toSqlKey followedId

          follow :: Follow
          follow = Follow (pId currentUser) followedKey

type Protected =
  JudgeablesAPI
  :<|> UsersAPI

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: ConnectionString -> AuthResult PresentationalUser -> Server Protected
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected connString (Authenticated currentUser) =
  judgeablesServer connString currentUser
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
  pure $ cors (\r -> corsPolicy) $ serveWithContext api
                          cfg
                          (server defaultCookieSettings jwtCfg connString)
    where corsPolicy = Just simpleCorsResourcePolicy { corsOrigins = Just (["http://127.0.0.1:1234", "http://localhost:1234"], True), corsRequestHeaders =  (corsRequestHeaders simpleCorsResourcePolicy) ++ ["content-type"] }


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
