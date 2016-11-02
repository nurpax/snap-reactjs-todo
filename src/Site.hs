{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Applicative
import           Control.Monad.Except
import           Control.Error.Safe (tryJust)
import           Control.Lens hiding ((.=))
import           Data.Aeson hiding (json)
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Data.Time (UTCTime)
import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple.JwtAuth as J
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import qualified Web.JWT as JWT
------------------------------------------------------------------------------
import           Application
import qualified Db
import           Util

type H = Handler App App

data PostTodoParams = PostTodoParams {
    ptId        :: Maybe Int
  , ptSavedOn   :: Maybe UTCTime
  , ptCompleted :: Bool
  , ptText      :: T.Text
  }

instance FromJSON PostTodoParams where
  parseJSON (Object v) = PostTodoParams <$> optional (v.: "id") <*> optional (v .: "savedOn") <*> v .: "completed" <*> v .: "text"
  parseJSON _          = mzero

instance ToJSON Db.Todo where
  toJSON c = object [ "id"        .= Db.todoId c
                    , "savedOn"   .= Db.todoSavedOn c
                    , "completed" .= Db.todoCompleted c
                    , "text"      .= Db.todoText c]

handleRestTodos :: H ()
handleRestTodos = (method GET listTodos) <|> (method POST saveTodo)
  where
    listTodos :: H ()
    listTodos = replyJson query
      where
        query (J.User uid _) = do
          withTop db $ Db.listTodos (Db.UserId uid)

    saveTodo = do
      ps <- reqJSON
      replyJson (query ps)
      where
        query ps (J.User uid _) =
          -- If the input todo id is Nothing, create a new todo.  Otherwise update
          -- an existing one.
          case ptId ps of
            Nothing -> do
              withTop db $ Db.newTodo (Db.UserId uid) (ptText ps)
            Just tid -> do
              let newTodo = Db.Todo tid (ptSavedOn ps) (ptCompleted ps) (ptText ps)
              withTop db $ Db.saveTodo (Db.UserId uid) newTodo

    replyJson :: ToJSON a => (J.User -> Handler App J.SqliteJwt a) -> H ()
    replyJson action = do
      res <- with jwt $ J.requireAuth action
      writeJSON res

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
           ("/api/login/new",  with jwt $ J.registerUser)
         , ("/api/login",      with jwt $ J.loginUser)
         , ("/api/todo",       handleRestTodos)
         , ("/static",         serveDirectory "static")
         , ("/",               serveFile "static/index.html")
         ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    -- Initialize auth that's backed by an sqlite database
    d <- nestSnaplet "db" db sqliteInit

    -- Initialize auth that's backed by an sqlite database
    jwt <- nestSnaplet "jwt" jwt (J.sqliteJwtInit d)

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn
    return $ App d jwt
