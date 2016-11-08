{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Snaplet.SqliteSimple.JwtAuth.Db where

import           Control.Concurrent
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as S
import           Snap
import           Snap.Snaplet.SqliteSimple

import           Snap.Snaplet.SqliteSimple.JwtAuth.Types

-- Used only internally in this module, shouldn't expose as this contains the
-- hashed password.
data DbUser = DbUser {
    dbuserId         :: Int
  , dbuserLogin      :: T.Text
  , dbuserHashedPass :: ByteString
  } deriving (Eq, Show)

instance FromRow DbUser where
  fromRow = DbUser <$> field <*> field <*> field

jwtAuthTable :: T.Text
jwtAuthTable = "jwt_auth"

versionTable :: T.Text
versionTable = T.concat [jwtAuthTable, "_version"]

userTable :: T.Text
userTable = T.concat [jwtAuthTable, "_user"]

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

executeSingle :: (ToRow q) => S.Query -> q -> H b ()
executeSingle q ps = do
  conn <- gets sqliteJwtConn
  liftIO $ withMVar conn $ \c ->
    S.execute c q ps

querySingle :: (ToRow q, FromRow a) => S.Query -> q -> H b (Maybe a)
querySingle q ps = do
  conn <- gets sqliteJwtConn
  liftIO $ withMVar conn $ \c ->
    return . listToMaybe =<< S.query c q ps

qconcat :: [T.Text] -> S.Query
qconcat = S.Query . T.concat

fromDbUser :: DbUser -> User
fromDbUser (DbUser i l _) = User i l

queryUser :: T.Text -> Handler b SqliteJwt (Maybe DbUser)
queryUser login = do
  querySingle (qconcat ["SELECT uid,login,password FROM ", userTable, " WHERE login=?"])
    (Only login)

insertUser :: T.Text -> ByteString -> Handler b SqliteJwt ()
insertUser login hashedPass = do
  let insq = qconcat ["INSERT INTO ", userTable, " (login,password) VALUES (?,?)"]
  executeSingle insq (login,hashedPass)
