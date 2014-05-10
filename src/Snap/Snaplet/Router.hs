{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Snaplet.Router
   ( RouterState (..)
   , HasRouter (..)
   , initRouter
   , urlPath
   , urlPathParams
   , redirectURL
   , redirectURLParams
   , routeWith
   , routeWithDebug
   , urlSplice
   , urlParamsSplice
   ) where


import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Text
import           Snap.Core (MonadSnap)
import qualified Snap.Core as SC
import           Snap.Snaplet
import           Web.Routes (PathInfo, toPathInfoParams, fromPathInfo)
------------------------------------------------------------------------------
import           Snap.Snaplet.Router.Heist
import           Snap.Snaplet.Router.URL
import           Snap.Snaplet.Router.Internal.Types



------------------------------------------------------------------------------
-- | Snaplet initializer.
initRouter :: Text  -- ^ Prefix to add to paths.
           -> SnapletInit b RouterState
initRouter prefix = makeSnaplet "router" "Snap router" Nothing $
    return $ RouterState prefix


------------------------------------------------------------------------------
-- | Given a routing function, routes matching requests or calls
-- 'Snap.Core.pass'.
routeWith :: (PathInfo url, MonadSnap m) =>
             (url -> m ()) -- ^ routing function
          -> m ()
routeWith = flip routeWithOr $ const SC.pass


------------------------------------------------------------------------------
-- | Given a routing function, routes matching requests or returns debugging
-- information. This is __not suitable for production__, but can be useful in
-- seeing what paths are available or determining why a path isn't routing as
-- expected.
routeWithDebug :: (PathInfo url, MonadSnap m) =>
                  (url -> m ()) -- ^ routing function
               -> m ()
routeWithDebug = flip routeWithOr (failIfNotLocal . SC.writeText)


------------------------------------------------------------------------------
routeWithOr
    :: (PathInfo url, MonadSnap m) => (url -> m ()) -> (Text -> m ()) -> m ()
routeWithOr router onLeft = do
    rqPath <- rqPathInfoNormalized
    case fromPathInfo rqPath of
         (Left e)    -> onLeft . pack $ e
         (Right url) -> router url

rqPathInfoNormalized
    :: (MonadSnap m) => m ByteString
rqPathInfoNormalized = do
    rq <- SC.getRequest
    return . dropTrailingSlashes $ SC.rqPathInfo rq
  where
    dropTrailingSlashes s =
        if ((B.singleton '/') `B.isSuffixOf` s)
        then dropTrailingSlashes $ B.init s
        else s
