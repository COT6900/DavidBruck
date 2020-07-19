module MovieSearchSite.Router
    ( router
    , renderRoute
    ) where

import MovieSearchSite.MovieRoute
    ( MovieRoute(..)
    )
import MovieSearchSite.Home
    ( homeController
    )
import MovieSearchSite.Search
    ( searchController
    )
import MovieSearchSite.Title
    ( titleController
    )
import Network.HTTP.Types
    ( renderQueryText
    )
import Data.Text
    ( append
    , pack
    )
import Data.Text.Encoding
    ( decodeUtf8
    )
import Blaze.ByteString.Builder
    ( toByteString
    )
import Control.Monad.Except
    ( ExceptT
    )
import Text.Read
    ( readMaybe
    )

router []       []                      = homeController
router []       [("q", query)]          = searchController query
router "title"  [("t", tconst)]
    | Just tconst <- readMaybe tconst   = titleController tconst

renderRoute route _ =
    case route of
        Home            -> pack "/"
        Search query    -> pack "/" `withQueryString` ("q", query)
        Title tconst    -> pack "/title" `withQueryString` ("t", show tconst)

    where
    withQueryString path (key, value) =
        path `append`
            decodeUtf8 (toByteString
                $ renderQueryText True [(pack key, Just $ pack value)])
