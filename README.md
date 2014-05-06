# snap-web-routes

## Type safe URLs for Snap

[`Snap web routes`](http://hackage.haskell.org/package/snap-web-routes) provides type safe URLs for Snap using [`web routes`](http://hackage.haskell.org/package/web-routes).

## How to use

The tutorial assumes you have a standard Snap app layout with an Application.hs and Site.hs. If your setup differs you'll need to adapt the instructions accordingly.

### `Application.hs`

To get going, you'll need to add a few things to `Application.hs`. This includes creating the URL data type and adding the routing function to our `App` data type.

```haskell
-- Enable a few extensions
{-# LANGUAGE FlexibleInstances #-} -- Needed by
{-# LANGUAGE TypeFamilies      #-} -- web-routes
{-# LANGUAGE DeriveGeneric     #-} -- Needed to derive Generic
                                   -- for our URL data type

-- Paths and params use Text.
import Data.Text (Text)

-- Snap.Web.Routes.Types exports everything you need to
-- define your PathInfo and MonadRoute instances.
import Snap.Web.Routes.Types

-- Your URL data type.  Deriving a `Generic` instance gives
-- you a `PathInfo` instance for free.
data AppUrl
    = Count Int
    | Echo Text
    | Paths [Text]
      deriving (Generic)

-- Extend your App type to include a routing function.
data App = App
    { _routeFn :: AppUrl -> [(Text, Maybe Text)] -> Text
    }

-- Thanks to the wonders of Generic, an empty instance
-- definition is all we need. Alternately, you can implement
-- toPathSegments and fromPathSegments yourself or use
-- web-routes-th.
instance PathInfo AppUrl

-- Set URL (Handler App App) to your URL data type defined above
-- and askRouteFn must point to the routing function you added to
-- your App.
instance MonadRoute (Handler App App) where
   type URL (Handler App App) = AppUrl
   askRouteFn = gets _routeFn
```

### `Site.hs`

Moving on to `Site.hs`, we'll setup handlers for each URL, as well initialise our app with a routing function.

```haskell
-- Snap.Web.Routes provides routing functions
import Snap.Web.Routes

-- Add your new routes using routeWith
routes :: [(ByteString, Handler App App ())]
routes = [ ("", routeWith routeAppUrl)
         , ("", serveDirectory "static")
         ]

-- Define handlers for each value constructor in your URL data type.
routeAppUrl :: AppUrl -> Handler App App ()
routeAppUrl appUrl =
    case appUrl of
      (Count n)   -> writeText $ ("Count = " `T.append` (T.pack $ show n))
      (Echo text) -> echo text
      (Paths ps)  -> writeText $ T.intercalate " " ps

-- You'll note that these are normal Snap handlers, except they can take
-- values from the value constructor as arguments. This is a lot nicer than
-- having to use getParam.
echo :: T.Text -> Handler App App ()
echo msg = heistLocal (bindString "message" msg) $ render "echo"

-- Add the routing function to your app.
app :: SnapletInit App App
app = makeSnaplet "app" "An example snap-web-routes app." Nothing $ do
    addRoutes routes
    return $ App renderRoute
```

If you prefixed the routes in routeWith (e.g. `("/prefix", routeWith routeAppUrl)`) then use `renderRouteWithPrefix` instead:

```haskell
return . App $ renderRouteWithPrefix "/prefix"
```

If you are having trouble figuring out why a particular request isn't routing as expected, try replacing `routeWith` with `routeWithDebug`. It'll display the available routes, as well as any failed route parses. Just remember that it's **not suitable for production** use.

### Rendering URLs

Helper functions are provided for rendering URLs:

```haskell
echo :: T.Text -> Handler App App ()
echo msg = do
    url <- showUrl $ Echo "this is a test"
    if msg == "test" then redirect (encodeUtf8 url) else renderEcho
  where
    renderEcho = heistLocal (I.bindSplices echoSplices) $ render "echo"
    echoSplices = do
        "message"  ## I.textSplice msg
        "countUrl" ## urlSplice (Count 10)
```

In the example above you'll find we use `urlSplice` to turn a URL into a splice, and `showUrl` to render a URL as Text.
