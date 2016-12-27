{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util (
    reqJSON
  , reqParam
  , writeJSON
  , jsonResponse
  , finishEarly
  ) where

import           Control.Error hiding (err)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import           Snap.Core
import qualified Snap.Snaplet.SqliteSimple.JwtAuth as J
import           Snap.Snaplet.SqliteSimple.JwtAuth (jsonResponse, reqJSON)

class FromParam a where
  parseParam:: B.ByteString -> Either B.ByteString a

instance (FromParam Int) where
  parseParam = reader T.decimal

tryGetParam :: MonadSnap m => B.ByteString -> m (Either B.ByteString B.ByteString)
tryGetParam p =
  justErr (Char8.pack ("missing get param '"++ show p ++"'")) <$> getParam p

reader :: T.Reader a -> B.ByteString -> Either B.ByteString a
reader p s =
  case p  (T.decodeUtf8 s) of
    Right (a, "") -> return a
    Right (_, _) -> Left "readParser: input not exhausted"
    Left e -> Left (Char8.pack e)

reqParam :: (MonadSnap m, FromParam a) => B.ByteString -> m a
reqParam name = do
  et <- tryGetParam name
  either (finishEarly 400) return (et >>= parseParam)

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
