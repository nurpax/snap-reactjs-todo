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
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteJwt as J
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import qualified Web.JWT as JWT
------------------------------------------------------------------------------
import           Application
import qualified Db
import           Util

type H = Handler App App

data LoginParams = LoginParams {
    lpLogin :: T.Text
  , lpPass  :: T.Text
  }

instance FromJSON LoginParams where
    parseJSON (Object v) = LoginParams <$>
                           v .: "login" <*>
                           v .: "pass"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

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

replyJwt :: Int -> T.Text -> H ()
replyJwt uid login =
  let cs = JWT.def {
            JWT.unregisteredClaims = M.fromList [("id", Number (fromIntegral uid)), ("login", String login)]
          }
      key = JWT.secret "site_secret"
      token = JWT.encodeSigned JWT.HS256 key cs in
  writeJSON $ object [ "token" .= token ]

handleNewUser :: H ()
handleNewUser = method POST newUser
  where
    newUser = runHttpErrorExceptT $ do
      login <- lift reqJSON
      user  <- lift $ with jwt $ J.createUser (lpLogin login) (lpPass login)
      u <- hoistHttpError (first show user)
      return $ replyJwt (J.userId u) (J.userLogin u)

handleLogin :: H ()
handleLogin = method POST go
  where
    go = runHttpErrorExceptT $ do
      login <- lift reqJSON
      user  <- lift $ with jwt $ J.loginUser (lpLogin login) (lpPass login)
      u <- hoistHttpError (first show user)
      return $ replyJwt (J.userId u) (J.userLogin u)

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
           ("/api/login/new", handleNewUser)
         , ("/api/login",     handleLogin)
         , ("/api/todo",      handleRestComments)
         , ("/build",         serveDirectory "static/build")
         , ("/",              serveFile "static/index.html")
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

