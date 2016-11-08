{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util (
    reqJSON
  , runHttpErrorExceptT
  , hoistHttpError
  , writeJSON
  , jsonResponse
  ) where

import           Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple.JwtAuth as J
import           Snap.Snaplet.SqliteSimple.JwtAuth (jsonResponse, reqJSON)
import           Application

type H = Handler App App

data HttpError = HttpError Int String

-------------------------------------------------------------------------------
-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (MonadSnap m, A.ToJSON a) => a -> m ()
writeJSON = J.writeJSON

-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> B.ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith

runHttpErrorExceptT :: ExceptT HttpError H (H ()) -> H ()
runHttpErrorExceptT e = runExceptT e >>= either err id
  where
    err (HttpError errCode msg) = do
      let m = T.encodeUtf8 . T.pack $ msg
      logError m
      finishEarly errCode m

badReq :: String -> HttpError
badReq msg = HttpError 400 msg

-- | Upgrade an 'Either' to an 'ExceptT'
hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . return

hoistHttpError :: Monad m => Either String a -> ExceptT HttpError m a
hoistHttpError (Left m)  = hoistEither . Left . badReq $ m
hoistHttpError (Right v) = hoistEither . Right $ v
