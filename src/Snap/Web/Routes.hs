-- |
-- This module provides a ready to use implementation of `web-routes` for Snap.
--
module Snap.Web.Routes
  ( routeWith
  , routeWithDebug
  , renderRoute
  , renderRouteWithPrefix
  , showUrl
  , showUrlParams
  , urlSplice
  ) where

import Snap.Web.Routes.App
import Snap.Web.Routes.Heist
import Snap.Web.Routes.Text
