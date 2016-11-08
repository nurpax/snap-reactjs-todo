
module Snap.Snaplet.SqliteSimple.JwtAuth (
  -- * The Snaplet
    SqliteJwt(..)
  -- * Types
  , User(..)
  , AuthFailure(..)
  , sqliteJwtInit
  -- * High-level handlers
  , requireAuth
  , registerUser
  , loginUser
  -- * Lower-level handlers that can be used to implement a more customized login
  , createUser
  , login
  -- * Utility functions (from the snap-extras package)
  , jsonResponse
  , writeJSON
  , reqJSON
  ) where

import Snap.Snaplet.SqliteSimple.JwtAuth.Types
import Snap.Snaplet.SqliteSimple.JwtAuth.JwtAuth
