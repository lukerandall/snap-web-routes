{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Snaplet.Router
   ( RouterState (..)
   , HasRouter (..)
   , initRouter
   ) where


import Control.Monad.State (lift)
import Data.Text
import Heist (HeistT)
import Snap.Web.Routes.App (renderRouteWithPrefix)
import Snap.Core (MonadSnap)
import Snap.Snaplet
import Web.Routes (PathInfo)


data RouterState = RouterState
    { _prefix :: Text
    }


class (Monad m) => HasRouter m where
    type URL m
    getRouterState :: m RouterState


------------------------------------------------------------------------------
-- | HasRouter instance for 'HeistT'.
instance (HasRouter m) => HasRouter (HeistT n m) where
    type URL (HeistT n m) = URL m
    getRouterState = lift getRouterState


initRouter :: Text -> SnapletInit b RouterState
initRouter prefix = makeSnaplet "router" "Snap router" Nothing $ do
    return $ RouterState prefix


showPath :: (HasRouter m, PathInfo (URL m)) => (URL m) -> m Text
showPath url = showPathParams url []

showPathParams :: (HasRouter m, PathInfo (URL m)) => (URL m) -> [(Text, Maybe Text)] -> m Text
showPathParams url params = do
    state <- getRouterState
    return $ renderRouteWithPrefix (_prefix state) url params
