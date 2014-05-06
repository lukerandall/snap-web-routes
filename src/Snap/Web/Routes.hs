-- |
-- This module provides a ready to use implementation of `web-routes` for Snap.
--
-- The tutorial assumes you have a standard Snap app with an Application.hs and
-- Site.hs.
--
-- To get going, you'll need to add a few things to Application.hs
--
-- @
-- -- Enable a few extensions
--
-- \-\- Needed to derive a generic instance for our URL data type
-- \{\-\# LANGUAGE DeriveGeneric     \#\-\}
--
-- \-\- Needed by web-routes
-- \{\-\# LANGUAGE FlexibleInstances \#\-\}
-- \{\-\# LANGUAGE TypeFamilies      \#\-\}
--
-- \-\- Paths and params are of type Text.
-- import Data.Text (Text)
--
-- \-\- Snap.Web.Routes.Types exports everything you need to
-- \-\- define your PathInfo and MonadRoute instances.
-- import Snap.Web.Routes.Types
--
-- \-\- Your URL data type.  Deriving a `Generic` instance gives
-- \-\- you a `PathInfo` instance for free.
-- data AppUrl
--     = Count Int
--     | Echo Text
--     | Paths [Text]
--       deriving (Generic)
--
-- \-\- Extend your App type to include a routing function.
-- data App = App
--     { _routeFn :: AppUrl -> [(Text, Maybe Text)] -> Text
--     }
--
-- \-\- Thanks to the wonders of Generic, an empty instance
-- \-\- definition is all we need. Alternately, you can implement
-- \-\- toPathSegments and fromPathSegments yourself or use
-- \-\- web-routes-th.
-- instance PathInfo AppUrl
--
-- \-\- Set URL (Handler App App) to your URL data type defined above
-- \-\- and askRouteFn must point to the routing function you added to
-- \-\- your App.
-- instance MonadRoute (Handler App App) where
--    type URL (Handler App App) = AppUrl
--    askRouteFn = gets _routeFn
-- @
--
-- Moving on to Site.hs.
--
-- @
-- \-\- Snap.Web.Routes provides routing functions
-- import Snap.Web.Routes
--
-- \-\- Add your new routes using routeWith
-- routes :: [(ByteString, Handler App App ())]
-- routes = [ ("", routeWith routeAppUrl)
--          , ("", serveDirectory "static")
--          ]
--
-- \-\- Define handlers for each value constructor in your URL data type.
-- routeAppUrl :: AppUrl -> Handler App App ()
-- routeAppUrl appUrl =
--     case appUrl of
--       (Count n)   -> writeText $ ("Count = " `T.append` (T.pack $ show n))
--       (Echo text) -> echo text
--       (Paths ps)  -> writeText $ T.intercalate " " ps
--
-- \-\- You'll note that these are normal Snap handlers, except they can take
-- \-\- values from the value constructor as arguments. This is a lot nicer than
-- \-\- having to use getParam.
-- echo :: T.Text -> Handler App App ()
-- echo msg = heistLocal (bindString "message" msg) $ render "echo"
--
-- \-\- Add the routing function to your app.
-- app :: SnapletInit App App
-- app = makeSnaplet "app" "An example snap-web-routes app." Nothing $ do
--     addRoutes routes
--     return $ App renderRoute
-- @
--
-- If you prefixed the routes in routeWith
--
-- > ("/prefix", routeWith routeAppUrl)
--
-- then use `renderRouteWithPrefix` instead:
--
-- > return . App $ renderRouteWithPrefix "/prefix"
--
-- If you are having trouble figuring out why a particular request isn't routing
-- as expected, try replacing 'routeWith' with 'routeWithDebug'. It'll display
-- the available routes, as well as any failed route parses. Just remember that
-- it's not suitable for production use.


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
