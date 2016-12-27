{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Db (
    User(..)
  , UserId
  , Todo(..)
  , Db
  , createTables
  , runDb
  , newTodo
  , saveTodo
  , listTodos) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson (ToJSON, toJSON, (.=), object)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.SQLite.Simple (Connection, FromRow, ToRow, Query, Only(..))
import qualified Database.SQLite.Simple as S

type UserId = Int
data DbContext = DbContext {
    connection :: Connection
  , user       :: UserId
  }

-- Reader/IO monad that provides a connection handle and current user for DB
-- entry points.
type Db = ReaderT DbContext IO

data User = User UserId T.Text

data Todo = Todo
  {
    todoId        :: Int
  , todoSavedOn   :: Maybe UTCTime
  , todoCompleted :: Bool
  , todoText      :: T.Text
  } deriving (Eq, Show)

instance FromRow Todo where
  fromRow = Todo <$> S.field <*> S.field <*> S.field <*> S.field

instance ToJSON Todo where
  toJSON c = object [ "id"        .= Db.todoId c
                    , "savedOn"   .= Db.todoSavedOn c
                    , "completed" .= Db.todoCompleted c
                    , "text"      .= Db.todoText c]


tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: Connection -> IO ()
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

runDb :: Connection -> User -> Db a -> IO a
runDb c (User uid _) a = runReaderT a (DbContext c uid)

insertRow :: ToRow p => Query -> p -> Db Int
insertRow sql params = do
  c <- asks connection
  liftIO (S.execute c sql params >> fromIntegral <$> S.lastInsertRowId c)

-- Same as SQLite.Simple.query, just running in the Db monad
query :: (ToRow p, FromRow r) => Query -> p -> Db [r]
query sql params = do
  c <- asks connection
  liftIO (S.query c sql params)

-- | Retrieve a user's list of todos
listTodos :: Db [Todo]
listTodos = do
  uid <- asks user
  query "SELECT id,saved_on,completed,todo FROM todos WHERE user_id = ?" (Only uid)

-- Query an existing todo
queryTodo :: Int -> Db Todo
queryTodo tid = do
  uid <- asks user
  [todo] <- query "SELECT id,saved_on,completed,todo FROM todos WHERE user_id = ? AND id = ?" (uid, tid)
  return todo

-- | Save a new todo for a user
newTodo :: T.Text -> Db Todo
newTodo c = do
  uid <- asks user
  insertRow "INSERT INTO todos (user_id,todo) VALUES (?,?)" (uid, c) >>= queryTodo

-- | Save a new todo for a user
saveTodo :: Todo -> Db (Either BS.ByteString Todo)
saveTodo todo = do
  uid <- asks user
  conn <- asks connection
  liftIO $ S.executeNamed conn "UPDATE todos SET saved_on=:so, completed=:c, todo=:text WHERE user_id = :uid AND id = :id"
    [ ":so"   S.:= todoSavedOn todo
    , ":c"    S.:= todoCompleted todo
    , ":text" S.:= todoText todo
    , ":uid"  S.:= uid
    , ":id"   S.:= todoId todo
    ]
  numChangedRows <- liftIO $ S.changes conn
  -- If the todo exists and is owned by this user, a single row should've
  -- changed by the UPDATE statement
  if numChangedRows == 1 then
    Right <$> queryTodo (todoId todo)
  else
    return . Left $ "Unknown todo or not owned by user"
