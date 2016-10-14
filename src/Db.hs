{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Db (
    User(..)
  , UserId(..)
  , Todo(..)
  , createTables
  , saveTodo
  , listTodos) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Application

newtype UserId = UserId Int
data User = User UserId T.Text

data Todo = Todo
  {
    todoId :: Int
  , todoSavedOn :: UTCTime
  , todoCompleted :: Bool
  , todoText :: T.Text
  } deriving (Show)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field

tableExists :: S.Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: S.Connection -> IO ()
createTables conn = do
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  schemaCreated <- tableExists conn "todos"
  unless schemaCreated $
    S.execute_ conn
      (S.Query $
       T.concat [ "CREATE TABLE todos ("
                , "id INTEGER PRIMARY KEY, "
                , "user_id INTEGER NOT NULL, "
                , "saved_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "completed INTEGER DEFAULT 0, "
                , "todo TEXT)"])

-- | Retrieve a user's list of todos
listTodos :: UserId -> Handler App Sqlite [Todo]
listTodos (UserId uid) =
  query "SELECT id,saved_on,completed,todo FROM todos WHERE user_id = ?" (Only uid)

queryTodo :: S.Connection -> UserId -> Int -> IO Todo
queryTodo conn (UserId uid) tid = do
  [todo] <- S.query conn "SELECT id,saved_on,completed,todo FROM todos WHERE user_id = ? AND id = ?" (uid, tid)
  liftIO (putStrLn . show $ todo)
  return todo

-- | Save a new todo for a user
saveTodo :: UserId -> T.Text -> Handler App Sqlite Todo
saveTodo user@(UserId uid) c = do
  withSqlite $ \conn -> do
    S.execute conn "INSERT INTO todos (user_id,todo) VALUES (?,?)" (uid, c)
    tid <- S.lastInsertRowId conn
    queryTodo conn user (fromIntegral tid)
