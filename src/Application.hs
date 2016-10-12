{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.SqliteSimple
import Snap.Snaplet.SqliteJwt

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _db :: Snaplet Sqlite
    , _jwt :: Snaplet SqliteJwt
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------
type AppHandler = Handler App App


