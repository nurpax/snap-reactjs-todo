{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.SqliteSimple.JwtAuth.Util where

import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import           Data.Int (Int64)
import           Snap
import           System.Directory (doesFileExist)
import           Web.ClientSession (randomKey)

-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> B.ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith

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

-- | Get a key from the given text file.
--
-- If the file does not exist, a random key will be generated and stored in
-- that file.
--
-- This code is borrowed from the clientsession package but it uses a
-- different signature.  We just need the raw ByteString.
getKey :: FilePath           -- ^ File name where key is stored.
       -> IO B.ByteString    -- ^ The actual key.
getKey keyFile = do
    exists <- doesFileExist keyFile
    if exists
        then B.readFile keyFile
        else newKey
  where
    newKey = do
        (bs, _) <- randomKey
        B.writeFile keyFile bs
        return bs
