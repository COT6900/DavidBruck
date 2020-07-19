{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.ErrorController
    ( errorController
    ) where

import MovieSearchSite.MovieRoute
    ( MovieRoute
    )
import MovieSearchSite.Exceptions
    ( MethodNotAllowedException(..)
    , NotFoundException(..)
    )
import MovieSearchSite.Response
    ( HtmlResponse(..)
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

errorController :: SomeException -> HtmlResponse
errorController err
    | Just (MethodNotAllowedException method) <- fromException err =
          HtmlResponse
          { respCode    = MethodNotAllowed
          , respTitle   = "Method Not Allowed"
          , respBody    = [hamlet|
                <h1>Method Not Allowed<br/>
                    <small>Method: #{show method}
            |]
          }
    | Just (NotFoundException path) <- fromException err =
          HtmlResponse
          { respCode    = NotFound
          , respTitle   = "Not Found"
          , respBody    = [hamlet|
                <h1>Not Found<br/>
                    <small>Path: /#{path}
            |]
          }
    | otherwise =
          HtmlResponse
          { respCode    = InternalServerError
          , respTitle   = "Unknown Error"
          , respBody    = [hamlet|
                <h1>Unknown Error
                    <small>#{show err}
            |]
          }
