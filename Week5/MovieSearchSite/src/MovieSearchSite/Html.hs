{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Html
    ( html
    ) where

import MovieSearchSite.Response
    ( HtmlResponse(..)
    )

import MovieSearchSite.Router
    ( renderRoute
    )
import MovieSearchSite.MovieRoute
    ( MovieRoute(..)
    )
import Network.HTTP.Server
    ( Response(..)
    , StatusCode(..)
    )
import Network.HTTP.Server.Response
    ( reason
    , statusCodeTriplet
    )
import Network.HTTP.Headers
    ( Header(..)
    , HeaderName(..)
    )
import Data.ByteString.Lazy
    ( length
    , ByteString
    )
import Data.Time.Format
    ( formatTime
    , defaultTimeLocale
    , FormatTime
    )
import Text.Hamlet
    ( hamlet
    )
import Text.Blaze.Html.Renderer.Utf8
    ( renderHtml
    )

html :: (FormatTime t) => HtmlResponse -> t -> Response ByteString
html (HtmlResponse code page body) now =
    Response
    { rspCode    = statusCodeTriplet code
    , rspReason  = reason code
    , rspHeaders = makeHeaders
    , rspBody    = resp
    }
    where
    resp =
        renderHtml $ [hamlet|
                $doctype 5
                    <html>
                        <head>
                            <title>Movie Search Site - #{page}
                            <link rel="icon" href="data:,">
                        <body>
                            <form action=@{Home}>Search: #
                                <input name="q"/>
                            <br>
                            ^{body}
                            <br>
                            <footer>
                                <b>COT6900 MovieSearchSite &copy; David Bruck
                                <br>
                                <a href="https://github.com/COT6900/DavidBruck/tree/master/Week5">
                                    https://github.com/COT6900/DavidBruck/tree/master/Week5
            |]
            renderRoute

    makeHeaders =
        [ Header HdrDate rfc1123Date
        , Header HdrExpires "-1"
        , Header HdrCacheControl "no-store"
        , Header HdrContentType "text/html; charset=utf-8"
        , Header HdrServer "COT6900 DavidBruck MovieSearchSite"
        , Header HdrContentLength $ show $
              Data.ByteString.Lazy.length resp
        ]

        where
        rfc1123Date =
            formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT" now
