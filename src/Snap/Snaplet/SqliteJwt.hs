{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Minimal proof of concept JWT authentication snaplet using snaplet-sqlite-
simple.

-}

module Snap.Snaplet.SqliteJwt (
  -- * The Snaplet
    SqliteJwt(..)
  -- * Types
  , User(..) -- TODO this shouldn't be exposed
  , AuthFailure(..)
  , sqliteJwtInit
  -- * High-level handlers
  , requireAuth
  , registerUser
  , loginUser
  -- * Lower
  , createUser
  , login
  -- * Utility functions
  , jsonResponse
  , writeJSON
  , reqJSON
--  , validateUser
  ) where

import           Prelude hiding (catch)

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Lens hiding ((.=), (??))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Error
import qualified Crypto.BCrypt as BC
import           Data.Aeson
import           Data.Aeson.Types (parseEither)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Configurator as C
import           Data.Int (Int64)
import           Data.Maybe
import           Data.Map as M
import           Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as LT
import qualified Database.SQLite.Simple as S
import           Snap
import           Snap.Snaplet.SqliteSimple
import qualified Web.JWT as JWT

data SqliteJwt = SqliteJwt {
    sqliteJwtConn :: MVar S.Connection
  }

data User = User {
    userId    :: Int
  , userLogin :: T.Text
  } deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Object v) = User <$> (v .: "id") <*> (v .: "login")
  parseJSON _          = mzero

instance ToJSON User where
  toJSON (User i l) = object [ "id" .= i, "login" .= l ]

data LoginParams = LoginParams {
    lpLogin :: T.Text
  , lpPass  :: T.Text
  }

instance FromJSON LoginParams where
  parseJSON (Object v) = LoginParams <$>
                         v .: "login" <*>
                         v .: "pass"
  parseJSON _          = mzero

-- Used only internally in this module, shouldn't expose as this contains the
-- hashed password.
data DbUser = DbUser {
    dbuserId         :: Int
  , dbuserLogin      :: T.Text
  , dbuserHashedPass :: ByteString
  } deriving (Eq, Show)

instance FromRow DbUser where
  fromRow = DbUser <$> field <*> field <*> field

data AuthFailure =
    UnknownUser
  | DuplicateLogin
  | WrongPassword
  deriving (Eq, Show)

data HttpError = HttpError Int String

bcryptPolicy = BC.fastBcryptHashingPolicy

jwtAuthTable :: T.Text
jwtAuthTable = "jwt_auth"

versionTable :: T.Text
versionTable = T.concat [jwtAuthTable, "_version"]

userTable :: T.Text
userTable = T.concat [jwtAuthTable, "_user"]

type H a b = Handler a SqliteJwt b

--runHttpErrorExceptT :: ExceptT HttpError H (H ()) -> Handler b SqliteJwt ()
runHttpErrorExceptT :: ExceptT HttpError (Handler b SqliteJwt) (Handler b SqliteJwt ()) -> Handler b SqliteJwt ()
runHttpErrorExceptT e = runExceptT e >>= either err id
  where
    err (HttpError errCode msg) = do
      let m = T.encodeUtf8 . T.pack $ msg
      logError m
      finishEarly errCode m

jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . encode $ a

-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body assuming it is not larger
-- than 50000 bytes.
reqJSON :: (MonadSnap m, FromJSON b) => m b
reqJSON = reqBoundedJSON 50000

-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body with a size up to N
-- bytes. If parsing fails for any reson, request is terminated early
-- and a server error is returned.
reqBoundedJSON
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m a
reqBoundedJSON n = do
  res <- getBoundedJSON n
  case res of
    Left e -> finishEarly 400 (BS8.pack e)
    Right a -> return a

-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- decode `fmap` readRequestBody (fromIntegral n)
  liftIO $ putStrLn (show bodyVal)
  return $ case bodyVal of
    Nothing -> Left "Can't find JSON data in POST body"
    Just v -> case fromJSON v of
                Error e -> Left e
                Success a -> Right a


------------------------------------------------------------------------------
-- | Initialize the snaplet
sqliteJwtInit :: Snaplet Sqlite -> SnapletInit b SqliteJwt
sqliteJwtInit db = makeSnaplet "sqlite-simple-jwt" description Nothing $ do
    let conn = sqliteConn $ db ^# snapletValue
    liftIO $ createTableIfMissing conn
    return $ SqliteJwt conn
  where
    description = "sqlite-simple jwt auth"

executeSingle :: (ToRow q) => S.Query -> q -> H b ()
executeSingle q ps = do
  conn <- gets sqliteJwtConn
  liftIO $ withMVar conn $ \conn ->
    S.execute conn q ps

querySingle :: (ToRow q, FromRow a) => S.Query -> q -> H b (Maybe a)
querySingle q ps = do
  conn <- gets sqliteJwtConn
  liftIO $ withMVar conn $ \conn ->
    return . listToMaybe =<< S.query conn q ps

qconcat :: [T.Text] -> S.Query
qconcat = S.Query . T.concat

fromDbUser :: DbUser -> User
fromDbUser (DbUser i l _) = User i l

queryUser :: T.Text -> Handler b SqliteJwt (Maybe DbUser)
queryUser login = do
  querySingle (qconcat ["SELECT uid,login,password FROM ", userTable, " WHERE login=?"])
    (Only login)

createUser :: T.Text -> T.Text -> Handler b SqliteJwt (Either AuthFailure User)
createUser login pass = do
  user <- queryUser login
  case user of
    Nothing -> do
      hashedPass <- liftIO $ BC.hashPasswordUsingPolicy bcryptPolicy (LT.encodeUtf8 pass)
      let insq = qconcat ["INSERT INTO ", userTable, " (login,password) VALUES (?,?)"]
      executeSingle insq (login,hashedPass)
      user <- queryUser login
      return (Right (fromDbUser . fromJust $ user))
    Just _ ->
      return (Left DuplicateLogin)

login :: T.Text -> T.Text -> Handler b SqliteJwt (Either AuthFailure User)
login login pass = do
  user <- queryUser login
  case user of
    Nothing ->
      return (Left UnknownUser)
    Just user -> do
      if BC.validatePassword (dbuserHashedPass user) (LT.encodeUtf8 pass) then
        passwordOk (fromDbUser user)
      else
        passwordFail

  where
    -- TODO this should return JWT
    passwordOk u = return (Right u)
    passwordFail = return (Left WrongPassword)

parseBearerJwt :: ByteString -> Either String T.Text
parseBearerJwt s =
  AP.parseOnly (AP.string "Bearer " *> payload) s
  where
    payload = LT.decodeUtf8 <$> AP.takeWhile1 (AP.inClass base64)
    base64 = "A-Za-z0-9+/_=.-"


-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith

-- TODO use a config parameter for site_secret
jwtFromUser :: User -> Handler b SqliteJwt JWT.JSON
jwtFromUser (User uid login) = do
  let cs = JWT.def {
            JWT.unregisteredClaims = M.fromList [("id", Number (fromIntegral uid)), ("login", String login)]
          }
      key = JWT.secret "site_secret"
  return $ JWT.encodeSigned JWT.HS256 key cs

-- Authorize against JWT and run the user action if JWT verification succeeds.
requireAuth :: (User -> Handler b SqliteJwt a) -> Handler b SqliteJwt a
requireAuth action = do
  -- TODO add configuration options + document how to generate it
  let siteSecret = JWT.secret "site_secret"
  req <- getRequest
  res <- runExceptT $ do
    authHdr     <- getHeader "Authorization" (rqHeaders req) ?? "Missing Authorization header"
    encPayload  <- hoistEither . parseBearerJwt $ authHdr
    jwt         <- JWT.decode encPayload     ?? "Malformed JWT"
    verifJwt    <- JWT.verify siteSecret jwt ?? "JWT verification failed"
    let unregClaims = JWT.unregisteredClaims (JWT.claims verifJwt)
    -- TODO verify expiration too
    user        <- hoistEither . parseEither parseJSON $ (toObject unregClaims)
    return user
  either (finishEarly 401 . BS8.pack) action res
  where
    toObject = Object . HM.fromList . M.toList

handleLoginError :: AuthFailure -> H b ()
handleLoginError err =
  case err of
    DuplicateLogin -> failLogin dupeError
    UnknownUser    -> failLogin failedPassOrUserError
    WrongPassword  -> failLogin failedPassOrUserError
  where
    dupeError             = "Duplicate login"
    failedPassOrUserError = "Unknown user or wrong password"

    failLogin :: T.Text -> H b ()
    failLogin err = do
      jsonResponse
      modifyResponse $ setResponseStatus 401 "bad login"
      writeJSON $ object [ "error" .= err]

loginOK :: User -> Handler b SqliteJwt ()
loginOK user = do
  jwt <- jwtFromUser user
  writeJSON $ object [ "token" .= jwt ]

registerUser :: Handler b SqliteJwt ()
registerUser = method POST newUser
  where
    newUser = runHttpErrorExceptT $ do
      params     <- lift reqJSON
      userOrErr  <- lift $ createUser (lpLogin params) (lpPass params)
      return (either handleLoginError loginOK userOrErr)

loginUser :: Handler b SqliteJwt ()
loginUser = method POST go
  where
    go = runHttpErrorExceptT $ do
      params     <- lift reqJSON
      userOrErr  <- lift $ login (lpLogin params) (lpPass params)
      return (either handleLoginError loginOK userOrErr)


schemaVersion :: S.Connection -> IO Int
schemaVersion conn = do
  versionExists <- tableExists conn versionTable
  if not versionExists
    then return 0
    else do
      [Only v] <- S.query_ conn (qconcat ["SELECT version FROM ", versionTable, " LIMIT 1"]) :: IO [Only Int]
      return v

tableExists :: S.Connection -> T.Text -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

setSchemaVersion :: S.Connection -> Int -> IO ()
setSchemaVersion conn v = do
  let q = S.Query $ T.concat ["UPDATE ", versionTable, " SET version = ?"]
  S.execute conn q (Only v)

upgradeSchema :: Connection -> Int -> IO ()
upgradeSchema conn fromVersion = do
  ver <- schemaVersion conn
  when (ver == fromVersion) (upgrade ver >> setSchemaVersion conn (fromVersion+1))
  where
    upgrade 0 = do
      S.execute_ conn (S.Query $ T.concat ["CREATE TABLE ", versionTable, " (version INTEGER)"])
      S.execute_ conn (S.Query $ T.concat ["INSERT INTO  ", versionTable, " VALUES (1)"])

    upgrade _ = error "unknown version"

createInitialSchema :: S.Connection -> IO ()
createInitialSchema conn = do
  let q = S.Query $ T.concat
          [ "CREATE TABLE ", userTable, " (uid INTEGER PRIMARY KEY,"
          , "login text UNIQUE NOT NULL,"
          , "password text,"
          , "created_on timestamp);"
          ]
  S.execute_ conn q

createTableIfMissing :: MVar S.Connection -> IO ()
createTableIfMissing connMVar =
    withMVar connMVar $ \conn -> do
      authTblExists <- tableExists conn userTable
      unless authTblExists $ createInitialSchema conn
      upgradeSchema conn 0
