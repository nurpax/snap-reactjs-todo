{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.SqliteSimple.JwtAuth.Util where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Error
import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString.Char8 as BS8
import           Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap
--import           Snap.Snaplet.SqliteSimple

import           Snap.Snaplet.SqliteSimple.JwtAuth.Types

-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith

runHttpErrorExceptT :: ExceptT HttpError (Handler b SqliteJwt) (Handler b SqliteJwt ()) -> Handler b SqliteJwt ()
runHttpErrorExceptT e = runExceptT e >>= either err id
  where
    err (HttpError errCode msg) = do
      let m = T.encodeUtf8 . T.pack $ msg
      logError m
      finishEarly errCode m

jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . encode $ a

-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body assuming it is not larger
-- than 50000 bytes.
reqJSON :: (MonadSnap m, FromJSON b) => m b
reqJSON = reqBoundedJSON 50000

-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body with a size up to N
-- bytes. If parsing fails for any reson, request is terminated early
-- and a server error is returned.
reqBoundedJSON
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m a
reqBoundedJSON n = do
  res <- getBoundedJSON n
  case res of
    Left e -> finishEarly 400 (BS8.pack e)
    Right a -> return a

-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- decode `fmap` readRequestBody (fromIntegral n)
  return $ case bodyVal of
    Nothing -> Left "Can't find JSON data in POST body"
    Just v -> case fromJSON v of
                Error e -> Left e
                Success a -> Right a
