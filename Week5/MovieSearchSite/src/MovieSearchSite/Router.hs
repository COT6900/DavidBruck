module MovieSearchSite.Router
    ( router
    , renderRoute
    , MovieRoute(..)
    ) where

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
    
data MovieRoute =
    Home
        | Search String
        | Title Int

router "/"      []              = homeController
router "/"      [("q", query)]  = searchController query
router "/title" [("t", tconst)]
    | Just tconst <- read tconst = titleController tconst

renderRoute route _ =
    case route of
        Home            -> withQueryString Nothing
        Search query    -> withQueryString $ Just ("q", query)
        Title tconst    -> withQueryString $ Just ("t", show tconst)

    where
    withQueryString Nothing             = pack "/"
    withQueryString (Just (key, value)) =
        withQueryString Nothing `append`
            decodeUtf8 (toByteString $ renderQueryText True [(pack key, Just $ pack value)])
