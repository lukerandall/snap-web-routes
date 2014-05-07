{-# LANGUAGE FlexibleContexts  #-}

module Snap.Snaplet.Router.URL
  ( urlPath
  , urlPathParams
  , redirectURL
  , redirectURLParams
  ) where


import           Data.Text
import           Data.Text.Encoding (encodeUtf8)
import           Snap.Core (MonadSnap)
import qualified Snap.Core as SC
import           Snap.Snaplet
import           Snap.Snaplet.Router.Internal.Types
import           Web.Routes (PathInfo, toPathInfoParams, fromPathInfo)


------------------------------------------------------------------------------
-- | Returns the path for the given URL
urlPath :: (HasRouter m, PathInfo (URL m)) => URL m   -- ^ URL data type
         -> m Path                                    -- ^ Path of route
urlPath u = urlPathParams u []


------------------------------------------------------------------------------
-- | Returns the path with query string for the given URL and params
urlPathParams :: (HasRouter m, PathInfo (URL m)) => URL m  -- ^ URL data type
               -> [(Text, Maybe Text)]                     -- ^ Params
               -> m Path                                   -- ^ Path of route
                                                           -- with params as
                                                           -- query string
urlPathParams u p = do
    state <- getRouterState
    return $ urlPathParamsWithPrefix (_prefix state) u p


------------------------------------------------------------------------------
-- | Redirect to the path for the given URL
redirectURL
    :: (HasRouter m, MonadSnap m, PathInfo (URL m)) =>
       URL m  -- ^ URL data type
    -> m ()
redirectURL u = redirectURLParams u []


------------------------------------------------------------------------------
-- | Redirect to the path for the given URL with params as query string
redirectURLParams
    :: (HasRouter m, MonadSnap m, PathInfo (URL m)) =>
       URL m                 -- ^ URL data type
    -> [(Text, Maybe Text)]  -- ^ Params
    -> m ()
redirectURLParams u p = SC.redirect . encodeUtf8 =<< urlPathParams u p


------------------------------------------------------------------------------
-- | Turn a route and params into a path with the given prefix.
urlPathParamsWithPrefix
    :: PathInfo url =>
       Text                 -- ^ Route prefix
    -> url                  -- ^ URL data type
    -> [(Text, Maybe Text)] -- ^ Params
    -> Path
urlPathParamsWithPrefix prefix u p = prefix `append` toPathInfoParams u p
