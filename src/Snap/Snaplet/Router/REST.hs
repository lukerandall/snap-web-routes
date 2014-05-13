-- | This module provides data types useful for declaring RESTful routes.
-- 'Resource' is provided for the common case of resource specified
-- by a unique identifier (for e.g. @\/products\/3@). 'SingletonResource'
-- is provided for resources that do not require an identifier. Typical
-- usage would be as follows:
--
-- @
-- data AppUrl
--     = User (Resource Int)
--     | Profile SingletonResource
--     | Login
--     | Logout
-- @
--
-- This now allows you to define handlers for different end points for
-- your resource.
-- @
-- routeAppUrl appUrl =
--     case appUrl of
--       Login          -> with auth handleLogin   -- \/login
--       Logout         -> with auth handleLogout  -- \/logout
--       User Index     -> handleUserIndex         -- \/user
--       User New       -> handleUserNew           -- \/user\/new
--       User (Show n)  -> handleUserShow n        -- \/user\/:Int
--       User (Edit n)  -> handleUserEdit n        -- \/user\/:Int\/edit
--       Profile ShowS  -> handleProfileShow       -- \/profile
--       Profile NewS   -> handleProfileNew        -- \/profile/new
--       Profile EditS  -> handleProfileEdit       -- \/profile/edit
-- @
--

{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Router.REST
    ( Resource (..)
    , SingletonResource (..)
    ) where


import Control.Applicative
import Web.Routes.PathInfo (PathInfo (..), segment, toPathSegments, fromPathSegments)


-- | A data type to represent RESTful resources that have a unique identifier.
-- The data type used as the identifier must be an instance of 'PathInfo'.
data (PathInfo id) => Resource id
    = Index
    | New
    | Show id
    | Edit id
      deriving (Eq, Show, Read)


-- | A data type to represent singleton RESTful resources. Generally these
-- are identified in some other way, often the user's session.
data SingletonResource
    = ShowS
    | NewS
    | EditS
      deriving (Eq, Show, Read)


instance (PathInfo id) => PathInfo (Resource id) where
    toPathSegments Index = []
    toPathSegments New   = ["new"]
    toPathSegments (Show n) = toPathSegments n
    toPathSegments (Edit n) = toPathSegments n ++ ["edit"]

    fromPathSegments =
            New <$ segment "new"
        <|> fromPathSegments <**> ((pure Edit <* segment "edit") <|> pure Show)
        <|> pure Index


instance PathInfo SingletonResource where
    toPathSegments ShowS = []
    toPathSegments NewS  = ["new"]
    toPathSegments EditS = ["edit"]

    fromPathSegments =
            NewS <$ segment "new"
        <|> EditS <$ segment "edit"
        <|> pure ShowS
