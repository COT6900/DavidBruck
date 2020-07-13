{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Home
    ( homeController
    ) where

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

homeController :: ExceptT SomeException IO (String, HtmlUrl a)
homeController =
    return
        ( "Home"
        , [hamlet|
                <h1>Home
          |]
        )
