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
  , sqliteJwtInit
  , createUser
--  , loginUser
--  , validateUser
  ) where

import           Prelude hiding (catch)

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Crypto.BCrypt as BC
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as LT
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Types
import           Snap
import           Snap.Snaplet.SqliteSimple

data User = User {
    userId       :: Int
  , userLogin    :: T.Text
  , userPassword :: ByteString
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

data AuthFailure =
    UnknownUser
  | DuplicateLogin
  deriving (Eq, Show)

data SqliteJwt = SqliteJwt
    { sqliteJwtConn :: MVar S.Connection
    }

bcryptPolicy = BC.fastBcryptHashingPolicy

jwtAuthTable :: T.Text
jwtAuthTable = "jwt_auth"

versionTable :: T.Text
versionTable = T.concat [jwtAuthTable, "_version"]

userTable :: T.Text
userTable = T.concat [jwtAuthTable, "_user"]

------------------------------------------------------------------------------
-- | Initialize the snaplet
sqliteJwtInit :: Snaplet Sqlite -> SnapletInit b SqliteJwt
sqliteJwtInit db = makeSnaplet "sqlite-simple-jwt" description Nothing $ do
    let conn = sqliteConn $ db ^# snapletValue
    liftIO $ createTableIfMissing conn
    return $ SqliteJwt conn
  where
    description = "sqlite-simple jwt auth"

executeSingle :: (ToRow q)
            => MVar S.Connection -> S.Query -> q -> IO ()
executeSingle pool q ps = withMVar pool $ \conn ->
  S.execute conn q ps

querySingle :: (ToRow q, FromRow a)
            => MVar S.Connection -> S.Query -> q -> IO (Maybe a)
querySingle pool q ps = withMVar pool $ \conn ->
  return . listToMaybe =<< S.query conn q ps

queryUser :: MVar S.Connection -> T.Text -> IO (Maybe User)
queryUser conn login = do
  querySingle conn (S.Query (T.concat ["SELECT uid,login,password FROM ", userTable, " WHERE login=?"]))
    (Only login)

createUser :: T.Text -> T.Text -> Handler b SqliteJwt (Either AuthFailure User)
createUser login pass = do
  conn <- gets sqliteJwtConn
  let q = S.Query (T.concat ["SELECT login FROM ", userTable, " WHERE login = ?"])
  user <- liftIO $ queryUser conn login
  case user of
    Nothing -> do
      hashedPass <- liftIO $ BC.hashPasswordUsingPolicy bcryptPolicy (LT.encodeUtf8 pass)
      let insq = S.Query (T.concat ["INSERT INTO ", userTable, " (login,password) VALUES (?,?)"])
      liftIO $ executeSingle conn insq (login,hashedPass)
      user <- liftIO $ queryUser conn login
      return (Right (fromJust user))
    Just _ ->
      return (Left DuplicateLogin)

schemaVersion :: S.Connection -> IO Int
schemaVersion conn = do
  versionExists <- tableExists conn versionTable
  if not versionExists
    then return 0
    else
    do
      let q = T.concat ["SELECT version FROM ", versionTable, " LIMIT 1"]
      [Only v] <- S.query_ conn (S.Query q) :: IO [Only Int]
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
