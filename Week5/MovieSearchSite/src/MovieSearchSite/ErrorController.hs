{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.ErrorController
    ( errorController
    ) where

import MovieSearchSite.Router
    ( MovieRoute
    )
import MovieSearchSite.Exceptions
    ( MethodNotAllowedException(..)
    , NotFoundException(..)
    )
import Network.HTTP.Server
    ( StatusCode(..)
    )
import Control.Exception
    ( fromException
    , SomeException
    )
import Text.Hamlet
    ( hamlet
    , HtmlUrl
    )

errorController :: SomeException -> (StatusCode, String, HtmlUrl MovieRoute)
errorController err
    | Just (MethodNotAllowedException method) <- fromException err =
          ( MethodNotAllowed
          , "Method Not Allowed"
          , [hamlet|
                <h1>Method Not Allowed<br/>
                    <small>Method: #{show method}
            |]
          )
    | Just (NotFoundException path) <- fromException err =
          ( NotFound
          , "Not Found"
          , [hamlet|
                <h1>Not Found<br/>
                    <small>Path: #{path}
            |]
          )
    | otherwise =
          ( InternalServerError
          , "Unknown Error"
          , [hamlet|
                <h1>Unknown Error
                    <small>#{show err}
            |]
          )
