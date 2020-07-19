{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Home
    ( homeController
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

homeController :: ExceptT SomeException IO HtmlResponse
homeController =
    do firstTitles <- getTitles 20 `withDatabase` "IMDB.db"
       return $ okResponse
           { respTitle = "Home"
           , respBody = [hamlet|
                 <h1>Home
                 ^{showTitles firstTitles}
             |]
           }
