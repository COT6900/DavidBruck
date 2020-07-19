module Main where

import MovieSearchSite.Controller
    ( controller
    )
import MovieSearchSite.ErrorController
    ( errorController
    )
import MovieSearchSite.Router
    ( router
    )
import MovieSearchSite.Html
    ( html
    )
import Network.HTTP.Server
    ( serverWith
    , defaultConfig
    , Config(..)
    , Response(..)
    , StatusCode(..)
    )
import Network.HTTP.Server.Logger
    ( stdLogger
    )
import Control.Monad.Except
    ( runExceptT
    )
import Data.Time.Clock
    ( getCurrentTime
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
                         either (return . errorController) return
           html result <$> getCurrentTime
