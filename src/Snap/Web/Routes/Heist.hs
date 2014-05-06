module Snap.Web.Routes.Heist
  ( urlSplice
  ) where

import Text.XmlHtml hiding (render)
import Web.Routes

------------------------------------------------------------------------------
-- | Render a url as a `Heist` splice.
urlSplice :: MonadRoute m => URL m -> m [Node]
urlSplice u =
    do t <- showURL u
       return [TextNode t]
