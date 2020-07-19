{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Search
    ( searchController
    ) where

import MovieSearchSite.Response
    ( okResponse
    , HtmlResponse(..)
    )
import MovieSearchSite.ShowTitles
    ( showTitles
    )
import MovieSearchSite.IMDBCrud
import DatabaseHelpers
    ( withDatabase
    )
import Control.Exception
    ( SomeException
    )
import Control.Monad.Except
    ( ExceptT
    )
import Text.Hamlet
    ( hamlet
    )

searchController :: String -> ExceptT SomeException IO HtmlResponse
searchController query =
    do matched <- searchTitles `withDatabase` "IMDB.db"
       return $ okResponse
           { respTitle = "Search"
           , respBody = [hamlet|
                 <h1>Search #{query}
                 ^{showTitles matched}
             |]
           }

    where
    searchTitles conn =
        do matchedIds <- map (fromIntegral . titlesSearch_rowid)
                             <$> getTitlesSearchByPrimaryTitle query 20 conn
           getTitlesByTconst matchedIds 20 conn
