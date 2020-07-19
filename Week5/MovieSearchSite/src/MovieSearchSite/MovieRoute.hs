module MovieSearchSite.MovieRoute
    ( MovieRoute(..)
    ) where
    
data MovieRoute =
    Home
  | Search String
  | Title Int
