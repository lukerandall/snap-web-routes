module Snap.Web.Routes.Heist
  ( heistUrl
  ) where

import Text.XmlHtml hiding (render)
import Web.Routes

------------------------------------------------------------------------------
-- | Render a url as a `Heist` splice.
heistUrl :: MonadRoute m => URL m -> m [Node]
heistUrl u =
    do t <- showURL u
       return [TextNode t]
