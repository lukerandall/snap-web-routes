module Snap.Web.Routes.Text
  ( showUrl
  , showUrlParams
  ) where

import Data.Text
import Web.Routes

showUrl :: MonadRoute m =>
           URL m -- ^ URL data type
        -> m Text -- ^ rendered route
showUrl = showURL

showUrlParams :: MonadRoute m =>
                 URL m -- ^ URL data type
              -> [(Text, Maybe Text)] -- ^ parameters
              -> m Text -- ^ rendered route
showUrlParams = showURLParams
