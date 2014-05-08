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

-- Used in HasRouter instances
import Control.Monad.State (get)

-- Paths and params use Text.
import Data.Text (Text)

-- Snap.Snaplet.Router.Types exports everything you need to
-- define your PathInfo and HasRouter instances.
import Snap.Snaplet.Router.Types

-- Your URL data type.  Deriving a `Generic` allows you to
-- get a free `PathInfo` instance.
data AppUrl
    = Count Int
    | Echo Text
    | Paths [Text]
      deriving (Generic)

-- Extend your App type to include the router snaplet.
data App = App
    { _heist :: Snaplet (Heist App)
    , _router :: Snaplet RouterState
    }

-- Thanks to Generic, an empty instance definition is all
-- you need. Alternately, you can implement 'toPathSegments'
-- and 'fromPathSegments' yourself or use web-routes-th.
instance PathInfo AppUrl

-- You need to define a HasRouter instance for your app.
-- You must set type URL (Handler App App) to the URL
-- data type you defined above. The router in
-- `with router` is the lens for the @RouterState@ snaplet
-- you added to App.
instance HasRouter (Handler App App) where
    type URL (Handler App App) = AppUrl
    getRouterState = with router get

-- You also need to define a HasRouter instance for the
-- router snaplet. Once again, set type URL (Handler b
-- RouterState) to the data type you defined above.
instance HasRouter (Handler b RouterState) where
    type URL (Handler b RouterState) = AppUrl
    getRouterState = get
```

### `Site.hs`

Moving on to `Site.hs`, we'll setup handlers for each URL, as well initialise our app with the router snaplet..

```haskell
-- Snap.Snaplet.Router provides routing functions
import Snap.Snaplet.Router

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

-- Add the router snaplet to your app.
app :: SnapletInit App App
app = makeSnaplet "app" "An example snap-web-routes app." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    r <- nestSnaplet "router" router $ initRouter ""
    addRoutes routes
    return $ App h r
```

The prefix you pass to the router snaplet must match the prefix you specified in routes, e.g. if it was `("/prefix", routeWith routeAppUrl)`) then:

```haskell
r <- nestSnaplet "router" router $ initRouter "/prefix"
```

If you are having trouble figuring out why a particular request isn't routing as expected, try replacing `routeWith` with `routeWithDebug`. It'll display the available routes, as well as any failed route parses. Just remember that it's **not suitable for production** use, and only displays debugging information for local requests.

### Rendering URLs

Helper functions are provided for rendering URLs:

```haskell
echo :: T.Text -> Handler App App ()
echo msg = do
    if msg == "test" then redirectURL (Echo "test passed") else renderEcho
  where
    renderEcho = heistLocal (I.bindSplices echoSplices) $ render "echo"
    echoSplices = do
        "message"  ## I.textSplice msg
        "countUrl" ## urlSplice (Count 10)
```

In the example above you'll find we use `urlSplice` to turn a URL into a splice, and `redirectURL` to redirect to a URL. There is also `urlPath` to render a URL as Text, as well as params versions of all these functions (`urlParamsSplice`, `redirectURLParams` and `urlPathParams`) that take a params list to append as a query string.
