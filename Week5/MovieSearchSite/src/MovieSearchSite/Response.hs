{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Response
    ( okResponse
    , HtmlResponse(..)
    ) where

import MovieSearchSite.MovieRoute
    ( MovieRoute
    )
import Network.HTTP.Server
    ( StatusCode(..)
    )
import Text.Hamlet
    ( hamlet
    , HtmlUrl
    )

data HtmlResponse = HtmlResponse
    { respCode  :: StatusCode
    , respTitle :: String
    , respBody  :: HtmlUrl MovieRoute
    }

okResponse = HtmlResponse
    { respCode  = OK
    , respTitle = "OK"
    , respBody  = [hamlet|
          <h1>OK
      |]
    }
