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
    = Login
    | Logout
    | Count Int
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
      (Login)     -> with auth handleLoginSubmit
      (Logout)    -> with auth handleLogout
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

### Using URLs

Let's look at how you can use your newly defined URL data type in your app. Firstly, you'll probably want to add links in Heist views. This is easily accomplished with the `urlSplice` and `urlParamsSplice` functions.

```haskell
linksHandler :: Handler App App ()
linksHandler = heistLocal (I.bindSplices linksSplices) $ render "links"
  where
    linksSplices = do
        "loginUrl" ## urlSplice Login
        "echoUrl"  ## urlSplice (Echo "ping")
        "countUrl" ## urlParamsSplice (Count 10) [("show-explanation", Just "true")]
```

As you can see, splicing URLs into Heist views is easily accomplished. You will likely also want to redirect to the handler for a certain URL. To do this we've got `redirectURL` and `redirectURLParams`. Let's look at an example.

```haskell
doSomethingHandler :: Handler App App ()
doSomethingHandler = doSomething >> redirectURL Logout
```

However, you will sometimes wish to redirect within a handler that runs in a snaplet other than the main app. With the router snaplet though, this is easily done:

```haskell
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> (withTop router $ redirectURL (Echo "logged out"))
```

Lastly, you can render a URL as Text with `urlPath` and `urlPathParams`.

```haskell
messageHandler :: Handler App App ()
messageHandler = do
    pathText <- urlPath (Echo "hello")
    heistLocal (I.bindSplices $ messageSplices path) $ render "message"
  where
    messageSplices path = do
        "message"  ## I.textSplice $ "The path you are lookin for is " `append` pathText
```

Remember, for each of `urlSplice`, `redirectURL` and `urlPath` there is a params version that takes a params list and will render the URL with the params as a query string.
