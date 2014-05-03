{-|

This module provides a ready to use implementation of `web-routes` for Snap.

To get going, you'll need to add a few things to `Application.hs`.

> {-# LANGUAGE DeriveGeneric     #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeFamilies      #-}

DeriveGeneric is used to derive the `PathInfo` instance for your URL data type,
the rest are needed by `web-routes`.

> import Data.Text (Text)
> import Snap.Web.Routes

"Snap.Web.Routes" exports the data types needed to define your `PathInfo` and
`MonadRoute` instances below.

> data AppUrl
>     = Count Int
>     | Echo Text
>     | Paths [Text]
>       deriving (Generic)

Define your application's URL data type. Deriving a `Generic` instance gives
you a `PathInfo` instance for free.

> data App = App
>     { _routeFn :: AppUrl -> [(Text, Maybe Text)] -> Text
>     }

Extend your App type to include a routing function.

> instance PathInfo AppUrl

Get your free PathInfo instance. Alternatives are to use `web-routes-th` or
implement PathInfo yourself.

> instance MonadRoute (Handler App App) where
>    type URL (Handler App App) = AppUrl
>    askRouteFn = gets _routeFn

Define your MonadRoute instance. In particular, `type URL (Handler App App)`
must be set to your URL data type defined above and `askRouteFn` should point
to the routing function you added to your App type.

Moving on to `Site.hs`.

> import Snap.Web.Routes

Snap.Web.Routes provides a convenience router function you'll need.

> routes :: [(ByteString, Handler App App ())]
> routes = [ ("",          serveDirectory "static")
>          , ("",          routeWith routeAppUrl)
>          ]

Add your routes to the bottom of the routes list using routeWith.

> routeAppUrl :: AppUrl -> Handler App App ()
> routeAppUrl appUrl =
>     case appUrl of
>       (Count n)   -> writeText $ ("Count = " `T.append` (T.pack $ show n))
>       (Echo text) -> echo text
>       (Paths ps)  -> writeText $ T.intercalate " " ps

> echo :: T.Text -> Handler App App ()
> echo msg = heistLocal (bindString "message" msg) $ render "echo"

Define the handler for each data constructor in your URL data type.

> app :: SnapletInit App App
> app = makeSnaplet "app" "An example application with snap-web-routes." Nothing $ do
>     addRoutes routes
>     return $ App renderRoute

Lastly, add the routing function to your app. If you prefixed the routes in routeWith:

>          , ("/prefix",          routeWith routeAppUrl)

then use `renderRouteWithPrefix` instead:

>     return . App $ renderRouteWithPrefix "/prefix"

|-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Web.Routes
  ( renderRoute
  , renderRouteWithPrefix
  , routeWith
  , heistUrl
  , gets
  , Generic
  , MonadRoute(..)
  , PathInfo(..)
  ) where

import Control.Monad.State (lift, gets)
import Data.Text (Text, append, pack)
import Heist (HeistT)
import Snap.Core
import Snap.Web.Routes.Heist
import Snap.Snaplet
import Web.Routes


instance (MonadRoute m) => MonadRoute (HeistT n m) where
    type URL (HeistT n m) = URL m
    askRouteFn = lift askRouteFn


routeWith :: (PathInfo url, MonadSnap m) => (url -> m ()) -> m ()
routeWith router =
    do rq <- getRequest
       case fromPathInfo $ rqPathInfo rq of
         (Left e) -> writeText (pack e)
         (Right url) -> router url


renderRoute :: PathInfo url => url -> [(Text, Maybe Text)] -> Text
renderRoute = renderRouteWithPrefix ""


renderRouteWithPrefix :: PathInfo url => Text -> url -> [(Text, Maybe Text)] -> Text
renderRouteWithPrefix prefix url params = prefix `append` toPathInfoParams url params
