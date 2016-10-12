{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
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
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteJwt as J
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import qualified Db
import           Util

type H = Handler App App

maybeWhen :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeWhen Nothing _  = return ()
maybeWhen (Just a) f = f a

handleRestComments :: H ()
handleRestComments = method GET listComments
  where
    listComments = do
      comments <- withTop db $ Db.listComments (Db.User 1 "")
      writeJSON $ map (\c ->
                        object [ "id"      .= Db.commentId c
                               , "savedOn" .= Db.commentSavedOn c
                               , "text"    .= Db.commentText c])
                    comments

handleNewUser :: H ()
handleNewUser = method POST newUser
  where
    newUser = do
      user <- with jwt $ J.createUser "jope" "jope123"
      liftIO $ putStrLn (show user)
      return ()

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
           ("/api/login",  handleNewUser)
         , ("/api/todo",   handleRestComments)
         , ("/build",      serveDirectory "static/build")
         , ("/",           serveFile "static/index.html")
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

