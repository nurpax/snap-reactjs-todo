{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.SqliteSimple.JwtAuth.Types where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Snap
import           Web.JWT as JWT (Secret)

data SqliteJwt = SqliteJwt {
    siteSecret    :: JWT.Secret
  , sqliteJwtConn :: MVar Connection
  }

data User = User {
    userId    :: Int
  , userLogin :: T.Text
  } deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Object v) = User <$> (v .: "id") <*> (v .: "login")
  parseJSON _          = mzero

instance ToJSON User where
  toJSON (User i l) = object [ "id" .= i, "login" .= l ]

data LoginParams = LoginParams {
    lpLogin :: T.Text
  , lpPass  :: T.Text
  }

instance FromJSON LoginParams where
  parseJSON (Object v) = LoginParams <$>
                         v .: "login" <*>
                         v .: "pass"
  parseJSON _          = mzero

data AuthFailure =
    UnknownUser
  | DuplicateLogin
  | WrongPassword
  deriving (Eq, Show)

data HttpError = HttpError Int String

type H a b = Handler a SqliteJwt b
