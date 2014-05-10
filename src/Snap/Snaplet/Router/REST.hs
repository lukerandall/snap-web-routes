{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Router.REST
    ( Resource (..)
    , SingularResource (..)
    ) where


import Control.Applicative
import GHC.Generics
import Web.Routes.PathInfo (PathInfo (..), segment, toPathSegments, fromPathSegments)


data Resource a
    = Index
    | New
    | Show a
    | Edit a
      deriving (Eq, Show, Read, Generic)


data SingularResource
    = ShowS
    | NewS
    | EditS
      deriving (Eq, Show, Read, Generic)


instance (PathInfo a) => PathInfo (Resource a) where
    toPathSegments Index = []
    toPathSegments New   = ["new"]
    toPathSegments (Show n) = toPathSegments n
    toPathSegments (Edit n) = toPathSegments n ++ ["edit"]

    fromPathSegments =
            New <$ segment "new"
        <|> fromPathSegments <**> ((pure Edit <* segment "edit") <|> pure Show)
        <|> pure Index


instance PathInfo SingularResource where
    toPathSegments ShowS = []
    toPathSegments NewS  = ["new"]
    toPathSegments EditS = ["edit"]

    fromPathSegments =
            NewS <$ segment "new"
        <|> EditS <$ segment "edit"
        <|> pure ShowS
