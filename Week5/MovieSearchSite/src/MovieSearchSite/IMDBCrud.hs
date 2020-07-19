{-# LANGUAGE TemplateHaskell #-}
module MovieSearchSite.IMDBCrud where

import CrudGenerator
    ( generateCrud 
    )

$(generateCrud "IMDB.db")
