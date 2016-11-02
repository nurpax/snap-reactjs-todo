{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util (
    logFail
  , reqJSON
  , logRunEitherT
  , runHttpErrorExceptT
  , hoistHttpError
  , writeJSON
  , jsonResponse
  ) where

import           Control.Monad.Except
import           Data.Int (Int64)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteJwt as J
import           Snap.Snaplet.SqliteJwt (jsonResponse, reqJSON)
import           Application

class ConvertParam c where
  parseParam :: B.ByteString -> Either String c

type H = Handler App App

data HttpError = HttpError Int String

-- | Log Either Left values or run the Handler action.  To be used in
-- situations where to user shouldn't see an error (either due to it
-- being irrelevant or due to security) but we want to leave a trace
-- of the error case anyway.
logFail :: Either String (H ()) -> H ()
logFail = either (logError . T.encodeUtf8 . T.pack) id


logRunEitherT :: ExceptT String H (H ()) -> H ()
logRunEitherT e = runExceptT e >>= logFail

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

forbiddenReq :: String -> HttpError
forbiddenReq msg = HttpError 403 msg

-- | Upgrade an 'Either' to an 'ExceptT'
hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . return

hoistHttpError :: Monad m => Either String a -> ExceptT HttpError m a
hoistHttpError (Left m)  = hoistEither . Left . badReq $ m
hoistHttpError (Right v) = hoistEither . Right $ v
