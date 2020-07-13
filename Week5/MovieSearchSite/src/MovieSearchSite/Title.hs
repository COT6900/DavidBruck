{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Title
    ( titleController
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

titleController :: Int -> ExceptT SomeException IO (String, HtmlUrl a)
titleController tconst =
    return
        ( "Title"
        , [hamlet|
                <h1>Title #{show tconst}
          |]
        )
