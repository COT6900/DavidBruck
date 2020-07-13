{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import MovieSearchSite.Controller
    ( controller
    )
import MovieSearchSite.ErrorController
    ( errorController
    )
import MovieSearchSite.Router
    ( router
    , renderRoute
    )
import Network.HTTP.Server
    ( serverWith
    , defaultConfig
    , Config(..)
    , Response(..)
    , StatusCode(..)
    )
import Network.HTTP.Server.Response
    ( reason
    , statusCodeTriplet
    )
import Network.HTTP.Server.Logger
    ( stdLogger
    )
import Network.HTTP.Headers
    ( Header(..)
    , HeaderName(..)
    )
import Control.Monad.Except
    ( runExceptT
    )
import Data.ByteString.Lazy
    ( length
    )
import Data.Time.Clock
    ( getCurrentTime
    )
import Data.Time.Format
    ( formatTime
    , defaultTimeLocale
    )
import Text.Hamlet
    ( hamlet
    )
import Text.Blaze.Html.Renderer.Utf8
    ( renderHtml
    )

main = serverWith
           defaultConfig
           { srvLog     = stdLogger
           , srvPort    = 8888
           }
           handler

    where
    handler _ url request =
        do result <- runExceptT (controller router url request) >>=
                         \case
                             Right (page, body) -> pure (OK, page, body)
                             Left err           -> return $ errorController err
           html result <$> getCurrentTime

        where
        html (code, page, body) now =
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
                                    ^{body}
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
