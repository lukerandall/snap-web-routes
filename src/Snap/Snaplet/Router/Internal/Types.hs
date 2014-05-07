{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Snaplet.Router.Internal.Types
  ( Path
  , RouterState (..)
  , HasRouter (..)
  ) where


import Control.Monad.State (lift)
import Data.Text
import Heist (HeistT)



type Path = Text


data RouterState = RouterState
    { _prefix :: Text
    }


------------------------------------------------------------------------------
-- | Instantiate this type class for @Handler App App@ and @Handler b
-- RouterState@ so that the snaplet can find its state. An instance requires
-- a type for @URL m@ - being the URL data type you have defined - and an
-- instance of @getRouterState@. Assuming your URL data type is called
-- @AppUrl@, the instance for @Handler b RouterState@ would be
--
-- @
-- instance HasRouter (Handler b RouterState) where
--     type URL (Handler b RouterState) = AppUrl
--     getRouterState = get
-- @
--
-- If the lens for the "Router" snaplet is @router@, your @Handler App App@
-- instance would be
--
-- @
-- instance HasRouter (Handler App App) where
--     type URL (Handler App App) = AppUrl
--     getRouterState = with router get
-- @

class (Monad m) => HasRouter m where
    type URL m
    getRouterState :: m RouterState


------------------------------------------------------------------------------
instance (HasRouter m) => HasRouter (HeistT n m) where
    type URL (HeistT n m) = URL m
    getRouterState = lift getRouterState
