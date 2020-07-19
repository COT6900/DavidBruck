{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Title
    ( titleController
    ) where

import MovieSearchSite.MovieRoute
    ( MovieRoute(..)
    )
import MovieSearchSite.Response
    ( okResponse
    , HtmlResponse(..)
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
    , HtmlUrl
    )

titleController :: Int -> ExceptT SomeException IO HtmlResponse
titleController tconst =
    getTitlesByTconst [fromIntegral tconst] 1 `withDatabase` "IMDB.db"
        >>= details

    where
    details [title] =
        do let primaryTitle = titles_primaryTitle title
           return $ okResponse
               { respTitle = primaryTitle ++ " Details"
               , respBody = [hamlet|
                     <form action=@{Home}>
                         <input type="submit" value="Back" />
                     <br>
                     <fieldset>
                         <legend>#{primaryTitle} Details
                         <label for="originalTitle">
                             <b>Original Title:
                         <span id="originalTitle">#{titles_originalTitle title}
                         <br>
                         <label for="type">
                             <b>Title Type:
                         <span id="type">#{titles_titleType title}
                         $maybe startYear   <- titles_startYear title
                             <br>
                             <label for="startYear">
                                 <b>Start Year:
                             <span id="startYear">#{startYear}
                         $maybe endYear     <- titles_endYear title 
                             <br>   
                             <label for="endYear">
                                 <b>End Year:
                             <span id="endYear">#{endYear}
                         $maybe runningTime <- titles_runtimeMinutes title
                             <br>
                             <label for="runningTime">
                                 <b>Running Time (minutes):
                             <span id="runningTime">#{runningTime}
                 |]
               }
