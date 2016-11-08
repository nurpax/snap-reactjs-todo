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

------------------------------------------------------------------------------
import           Control.Lens hiding ((.=), (??))
import           Control.Monad.Except
import           Control.Monad.State (gets)
import           Control.Error hiding (err)
import qualified Crypto.BCrypt as BC
import           Data.Aeson
import           Data.Aeson.Types (parseEither)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe
import           Data.Map as M
import           Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as LT
import           Snap
import           Snap.Snaplet.SqliteSimple (Sqlite, sqliteConn)
import qualified Web.JWT as JWT

import           Snap.Snaplet.SqliteSimple.JwtAuth.Util
import           Snap.Snaplet.SqliteSimple.JwtAuth.Types
import qualified Snap.Snaplet.SqliteSimple.JwtAuth.Db as Db

bcryptPolicy :: BC.HashingPolicy
bcryptPolicy = BC.fastBcryptHashingPolicy

-------------------------------------------------------------------------
-- | Initializer for the sqlite-simple JwtAuth snaplet.
sqliteJwtInit
  :: String         -- ^ JWT secret signing key filename
  -> Snaplet Sqlite -- ^ The sqlite-simple snaplet
  -> SnapletInit b SqliteJwt
sqliteJwtInit jwtSigningKeyFname db = makeSnaplet "sqlite-simple-jwt" description Nothing $ do
    k <- liftIO $ (JWT.binarySecret <$> getKey jwtSigningKeyFname)
    let conn = sqliteConn $ db ^# snapletValue
    liftIO $ Db.createTableIfMissing conn
    return $ SqliteJwt k conn
  where
    description = "sqlite-simple jwt auth"

-------------------------------------------------------------------------
-- | Create a new user.
createUser
  :: T.Text -- ^ Login name of the user to be created
  -> T.Text -- ^ Password of the new user
  -> Handler b SqliteJwt (Either AuthFailure User)
createUser loginName password = do
  user <- Db.queryUser loginName
  case user of
    Nothing -> do
      hashedPass <- liftIO $ BC.hashPasswordUsingPolicy bcryptPolicy (LT.encodeUtf8 password)
      -- TODO don't use fromJust
      Db.insertUser loginName (fromJust hashedPass)
      u <- Db.queryUser loginName
      return (Right (Db.fromDbUser . fromJust $ u))
    Just _ ->
      return (Left DuplicateLogin)

-------------------------------------------------------------------------
-- | Login a user
login
  :: T.Text -- ^ Login name of the user logging in
  -> T.Text -- ^ Password
  -> Handler b SqliteJwt (Either AuthFailure User)
login loginName password = do
  user <- Db.queryUser loginName
  case user of
    Nothing ->
      return (Left UnknownUser)
    Just u -> do
      if BC.validatePassword (Db.dbuserHashedPass u) (LT.encodeUtf8 password) then
        passwordOk (Db.fromDbUser u)
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
jwtFromUser (User uid loginName) = do
  key <- gets siteSecret
  let cs = JWT.def {
            JWT.unregisteredClaims = M.fromList [("id", Number (fromIntegral uid)), ("login", String loginName)]
          }
  return $ JWT.encodeSigned JWT.HS256 key cs

-- Authorize against JWT and run the user action if JWT verification succeeds.
requireAuth :: (User -> Handler b SqliteJwt a) -> Handler b SqliteJwt a
requireAuth action = do
  key <- gets siteSecret
  req <- getRequest
  res <- runExceptT $ do
    authHdr     <- getHeader "Authorization" (rqHeaders req) ?? "Missing Authorization header"
    encPayload  <- hoistEither . parseBearerJwt $ authHdr
    jwt         <- JWT.decode encPayload     ?? "Malformed JWT"
    verifJwt    <- JWT.verify key jwt        ?? "JWT verification failed"
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
    failLogin msg = do
      jsonResponse
      modifyResponse $ setResponseStatus 401 "bad login"
      writeJSON $ object [ "error" .= msg]

loginOK :: User -> Handler b SqliteJwt ()
loginOK user = do
  jwt <- jwtFromUser user
  writeJSON $ object [ "token" .= jwt ]

registerUser :: Handler b SqliteJwt ()
registerUser = method POST newUser
  where
    newUser = do
      params    <- reqJSON
      userOrErr <- createUser (lpLogin params) (lpPass params)
      either handleLoginError loginOK userOrErr

loginUser :: Handler b SqliteJwt ()
loginUser = method POST $ do
  params    <- reqJSON
  userOrErr <- login (lpLogin params) (lpPass params)
  either handleLoginError loginOK userOrErr
