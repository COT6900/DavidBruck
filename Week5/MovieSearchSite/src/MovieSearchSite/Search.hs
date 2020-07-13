{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Search
    ( searchController
    ) where

import Control.Exception
    ( SomeException
    )
import Control.Monad.Except
    ( ExceptT
    )
import Text.Hamlet
    ( hamlet
    , HtmlUrl
    )

searchController :: String -> ExceptT SomeException IO (String, HtmlUrl a)
searchController query =
    return
        ( "Search"
        , [hamlet|
                <h1>Search #{query}
          |]
        )
