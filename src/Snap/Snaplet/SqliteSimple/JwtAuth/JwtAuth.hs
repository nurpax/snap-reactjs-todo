{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Minimal proof of concept JWT authentication snaplet using snaplet-sqlite-
simple.

-}

module Snap.Snaplet.SqliteSimple.JwtAuth.JwtAuth (
    SqliteJwt(..)
  , User(..)
  , AuthFailure(..)
  , sqliteJwtInit
  , requireAuth
  , registerUser
  , loginUser
  , createUser
  , login
  , jsonResponse
  , writeJSON
  , reqJSON
  ) where

import           Prelude hiding (catch)

------------------------------------------------------------------------------
import           Control.Lens hiding ((.=), (??))
import           Control.Monad
import           Control.Monad.Except
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
import           Snap
import           Snap.Snaplet.SqliteSimple (Sqlite, sqliteConn)
import qualified Web.JWT as JWT

import           Snap.Snaplet.SqliteSimple.JwtAuth.Util
import           Snap.Snaplet.SqliteSimple.JwtAuth.Types
import qualified Snap.Snaplet.SqliteSimple.JwtAuth.Db as Db

bcryptPolicy = BC.fastBcryptHashingPolicy

-------------------------------------------------------------------------
-- | Initializer for the sqlite-simple JwtAuth snaplet.
sqliteJwtInit
  :: Snaplet Sqlite -- ^ The sqlite-simple snaplet
  -> SnapletInit b SqliteJwt
sqliteJwtInit db = makeSnaplet "sqlite-simple-jwt" description Nothing $ do
    let conn = sqliteConn $ db ^# snapletValue
    liftIO $ Db.createTableIfMissing conn
    return $ SqliteJwt conn
  where
    description = "sqlite-simple jwt auth"

-------------------------------------------------------------------------
-- | Create a new user.
createUser
  :: T.Text -- ^ Login name of the user to be created
  -> T.Text -- ^ Password of the new user
  -> Handler b SqliteJwt (Either AuthFailure User)
createUser login pass = do
  user <- Db.queryUser login
  case user of
    Nothing -> do
      hashedPass <- liftIO $ BC.hashPasswordUsingPolicy bcryptPolicy (LT.encodeUtf8 pass)
      -- TODO don't use fromJust
      Db.insertUser login (fromJust hashedPass)
      user <- Db.queryUser login
      return (Right (Db.fromDbUser . fromJust $ user))
    Just _ ->
      return (Left DuplicateLogin)

-------------------------------------------------------------------------
-- | Login a user
login
  :: T.Text -- ^ Login name of the user logging in
  -> T.Text -- ^ Password
  -> Handler b SqliteJwt (Either AuthFailure User)
login login pass = do
  user <- Db.queryUser login
  case user of
    Nothing ->
      return (Left UnknownUser)
    Just user -> do
      if BC.validatePassword (Db.dbuserHashedPass user) (LT.encodeUtf8 pass) then
        passwordOk (Db.fromDbUser user)
      else
        passwordFail

  where
    -- TODO this should return JWT
    passwordOk u = return (Right u)
    passwordFail = return (Left WrongPassword)

parseBearerJwt :: ByteString -> Either String T.Text
parseBearerJwt s = AP.parseOnly (AP.string "Bearer " *> payload) s
  where
    payload = LT.decodeUtf8 <$> AP.takeWhile1 (AP.inClass base64)
    base64 = "A-Za-z0-9+/_=.-"


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
