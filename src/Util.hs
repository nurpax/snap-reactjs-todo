{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util (
    reader
  , logFail
  , logRunEitherT
  , runHttpErrorExceptT
  , hoistHttpError
  , writeJSON
  , requirePostParam
  ) where

import           Control.Monad.Except
import qualified Data.Aeson as A
import           Data.ByteString
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import           Snap.Core
import           Snap.Snaplet
import           Application

class ConvertParam c where
  parseParam :: ByteString -> Either String c

type H = Handler App App

data HttpError = HttpError Int String

reader :: T.Reader a -> T.Text -> Either String a
reader p s =
  case p s of
    Right (a, "") -> return a
    Right (_, _) -> Left "readParser: input not exhausted"
    Left e -> Left e

-- | Log Either Left values or run the Handler action.  To be used in
-- situations where to user shouldn't see an error (either due to it
-- being irrelevant or due to security) but we want to leave a trace
-- of the error case anyway.
logFail :: Either String (H ()) -> H ()
logFail = either (logError . T.encodeUtf8 . T.pack) id


logRunEitherT :: ExceptT String H (H ()) -> H ()
logRunEitherT e = runExceptT e >>= logFail

-------------------------------------------------------------------------------
-- | Mark response as 'application/json'
jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"

-------------------------------------------------------------------------------
-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (MonadSnap m, A.ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . A.encode $ a

-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> ByteString -> m b
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

tryJust :: String -> Maybe a -> Either String a
tryJust s Nothing  = Left s
tryJust _ (Just v) = Right v

tryPostParam :: MonadSnap m => ByteString -> ExceptT HttpError m ByteString
tryPostParam p = do
  v <- lift $ getPostParam p
  hoistHttpError (tryJust ("missing POST param '" ++ show p ++ "'") v)

instance ConvertParam T.Text where
  parseParam v = Right . T.decodeUtf8 $ v

instance ConvertParam Int where
  parseParam v = (reader T.decimal . T.decodeUtf8 $ v)

requirePostParam :: (MonadSnap m, ConvertParam a) => ByteString -> ExceptT HttpError m a
requirePostParam n =
  tryPostParam n >>= \p -> hoistHttpError (parseParam p)
