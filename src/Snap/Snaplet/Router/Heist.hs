{-# LANGUAGE FlexibleContexts  #-}

module Snap.Snaplet.Router.Heist
  ( urlSplice
  , urlParamsSplice
  ) where


import Data.Text
import Snap.Snaplet.Router.Internal.Types
import Snap.Snaplet.Router.URL (urlPathParams)
import Text.XmlHtml hiding (render)
import Web.Routes (PathInfo)


------------------------------------------------------------------------------
-- | Returns the given URL as a `Heist` splice
urlSplice
    :: (HasRouter m, PathInfo (URL m)) => URL m  -- ^ URL data type
    -> m [Node]
urlSplice u = urlParamsSplice u []


------------------------------------------------------------------------------
-- | Returns the given URL as a `Heist` splice with query string for params
urlParamsSplice
    :: (HasRouter m, PathInfo (URL m)) => URL m  -- ^ URL data type
    -> [(Text, Maybe Text)]                      -- ^ Params
    -> m [Node]
urlParamsSplice u p = do
    t <- urlPathParams u p
    return [TextNode t]
