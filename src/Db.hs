{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Db (
    User(..)
  , UserId(..)
  , Todo(..)
  , createTables
  , newTodo
  , saveTodo
  , listTodos) where

import           Control.Monad
import           Data.Aeson (ToJSON, toJSON, (.=), object)
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
    todoId        :: Int
  , todoSavedOn   :: Maybe UTCTime
  , todoCompleted :: Bool
  , todoText      :: T.Text
  } deriving (Eq, Show)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field

instance ToJSON Todo where
  toJSON c = object [ "id"        .= Db.todoId c
                    , "savedOn"   .= Db.todoSavedOn c
                    , "completed" .= Db.todoCompleted c
                    , "text"      .= Db.todoText c]


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

-- Query an existing todo
queryTodo :: S.Connection -> UserId -> Int -> IO Todo
queryTodo conn (UserId uid) tid = do
  [todo] <- S.query conn "SELECT id,saved_on,completed,todo FROM todos WHERE user_id = ? AND id = ?" (uid, tid)
  return todo

-- | Save a new todo for a user
newTodo :: UserId -> T.Text -> Handler App Sqlite Todo
newTodo user@(UserId uid) c = do
  withSqlite $ \conn -> do
    S.execute conn "INSERT INTO todos (user_id,todo) VALUES (?,?)" (uid, c)
    tid <- S.lastInsertRowId conn
    queryTodo conn user (fromIntegral tid)

-- | Save a new todo for a user
saveTodo :: UserId -> Todo -> Handler App Sqlite Todo
saveTodo user@(UserId uid) todo = do
  withSqlite $ \conn -> do
    S.executeNamed conn "UPDATE todos SET saved_on=:so, completed=:c, todo=:text WHERE user_id = :uid AND id = :id"
      [ ":so"   S.:= todoSavedOn todo
      , ":c"    S.:= todoCompleted todo
      , ":text" S.:= todoText todo
      , ":uid"  S.:= uid
      , ":id"   S.:= todoId todo
      ]
    queryTodo conn user (todoId todo)
