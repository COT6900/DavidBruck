{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.ShowTitles
    ( showTitles
    ) where

import MovieSearchSite.MovieRoute
    ( MovieRoute(..)
    )
import MovieSearchSite.IMDBCrud
import Text.Hamlet
    ( hamlet
    )

showTitles titles =
    [hamlet|
        $if null titles
            <div>
                <b>No Titles
        $else
            <div>
                <b>
                    $if length titles == 20
                        First #
                    #{length titles} Titles:
            <table>
                <thead>
                    <tr style="font-weight:bolder">^{headers}
                <tbody>
                    $forall title <- titles
                        <tr>^{showTitle title}
    |]

    where
    headers                     =
        titleRow Nothing "Title" "Type"

    showTitle title             =
        titleRow
            (Just $ titles_tconst title)
            (titles_primaryTitle title)
            (titles_titleType title)

    titleRow tconst title type' = [hamlet|
        <td>
            $maybe tconst <- tconst
                <a href=@{Title tconst}>Click here for Details
        <td>#{title}
        <td>#{type'}
    |]
