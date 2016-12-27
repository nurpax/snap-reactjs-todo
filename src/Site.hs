{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, RecordWildCards #-}

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
import           Control.Lens hiding ((.=))
import           Data.Aeson hiding (json)
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           GHC.Generics
import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple.JwtAuth as J
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import qualified Db
import           Util

type H = Handler App App

data PostTodoParams = PostTodoParams {
    savedOn   :: Maybe UTCTime
  , completed :: Bool
  , text      :: T.Text
  } deriving (Generic)

instance FromJSON PostTodoParams

replyJson :: ToJSON a => (J.User -> Handler App J.SqliteJwt (Either ByteString a)) -> H ()
replyJson action = do
  res <- with jwt $ J.requireAuth action
  either (finishEarly 403) writeJSON res

handleRestUserInfo :: H ()
handleRestUserInfo = method GET (replyJson userInfo)
  where
    userInfo (J.User uid login) = return . Right $ object ["id" .= uid, "login" .= login]

handleRestListTodos :: H ()
handleRestListTodos =
  replyJson $ \(J.User uid _) ->
    withTop db $ Right <$> Db.listTodos (Db.UserId uid)

handleRestNewTodo :: H ()
handleRestNewTodo = do
  ps <- reqJSON
  replyJson (newTodo ps)
  where
    newTodo PostTodoParams{..} (J.User uid _) =
      withTop db $ Right <$> Db.newTodo (Db.UserId uid) text

handleRestUpdateTodo :: H ()
handleRestUpdateTodo = do
  ps     <- reqJSON
  todoId <- reqParam "id"
  replyJson (updateTodo ps todoId)
  where
    updateTodo PostTodoParams{..} tid (J.User uid _) = do
      let newTodo = Db.Todo tid savedOn completed text
      withTop db $ Db.saveTodo (Db.UserId uid) newTodo

handleUnknownAPI :: H ()
handleUnknownAPI = method GET err <|> method POST err <|> method PUT err
  where
    err = finishEarly 404 "Unknown API endpoint"

apiRoutes :: [(ByteString, Handler App App ())]
apiRoutes = [ ("/api/user",       handleRestUserInfo)
            , ("/api/todo",       method GET  handleRestListTodos)
            , ("/api/todo/:id",   method POST handleRestUpdateTodo)
            , ("/api/todo",       method POST handleRestNewTodo)
             ]

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/api/login/new",  with jwt J.registerUser)
         , ("/api/login",      with jwt J.loginUser)
         ]
         ++ apiRoutes ++
         [ ("/api",            handleUnknownAPI)
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
    j <- nestSnaplet "jwt" jwt (J.sqliteJwtInit J.defaults d)

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn
    return $ App d j
