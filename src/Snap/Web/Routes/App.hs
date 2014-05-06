{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Web.Routes.App
  ( routeWith
  , routeWithDebug
  , renderRoute
  , renderRouteWithPrefix
  ) where


import Control.Monad.State (lift, gets)
import Data.Text (Text, append, pack)
import Heist (HeistT)
import Snap.Core
import Snap.Snaplet
import Web.Routes


------------------------------------------------------------------------------
-- | Given a routing function, routes matching requests or calls
-- 'Snap.Core.pass'.
routeWith :: (PathInfo url, MonadSnap m) =>
             (url -> m ()) -- ^ routing function
          -> m ()
routeWith = flip routeWithOr $ const pass


------------------------------------------------------------------------------
-- | Given a routing function, routes matching requests or returns debugging
-- information. This is __not suitable for production__, but can be useful in
-- seeing what paths are available or determining why a path isn't routing as
-- expected.
routeWithDebug :: (PathInfo url, MonadSnap m) =>
                  (url -> m ()) -- ^ routing function
               -> m ()
routeWithDebug = flip routeWithOr (\err -> writeText err)


------------------------------------------------------------------------------
routeWithOr
    :: (PathInfo url, MonadSnap m) => (url -> m ()) -> (Text -> m ()) -> m ()
routeWithOr router onLeft =
    do rq <- getRequest
       case fromPathInfo $ rqPathInfo rq of
         (Left e) -> onLeft . pack $ e
         (Right url) -> router url


------------------------------------------------------------------------------
-- | Turn a route and params into a path.
renderRoute :: PathInfo url =>
               url -- ^ URL data type
            -> [(Text, Maybe Text)] -- ^ parameters
            -> Text -- ^ rendered route
renderRoute = renderRouteWithPrefix ""


------------------------------------------------------------------------------
-- | Turn a route and params into a path with the given prefix.
renderRouteWithPrefix
    :: PathInfo url =>
       Text -- ^ route prefix
    -> url -- ^ URL data type
    -> [(Text, Maybe Text)] -- ^ parameters
    -> Text -- ^ rendered route
renderRouteWithPrefix p u params = p `append` toPathInfoParams u params


------------------------------------------------------------------------------
-- | MonadRoute instance for 'HeistT'.
instance (MonadRoute m) => MonadRoute (HeistT n m) where
    type URL (HeistT n m) = URL m
    askRouteFn = lift askRouteFn
