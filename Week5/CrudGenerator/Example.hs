{-|
    CrudGenerator Example usage

    Prerequisites: build an IMDB.db or change "IMDB.db" in this file to point
                   to another SQLite database

    To build IMDB.db prerequisite, change directory to Week5 folder, and run
    (without apostrophes):
        'cabal build InitializeDatabase'
    Then, change directory to CrudGenerator folder, and run:
        'cabal run InitializeDatabase'

    To test Template Haskell CRUD exports, start GHCI with:
        'stack exec -- ghci -XTemplateHaskell'
    Then ':cd' to the current directory, and run two commands:
        ':load src/CrudGenerator Example'
        ':module Control.Monad.Except DatabaseHelpers CrudGenerator Example'

    Select first 100 rows with Haskell (still in GHCI):
        runExceptT $ (getTitles 100) `withDatabase` "IMDB.db"

    To see the fields and their types inside of a table's data type,
    just change Titles to the desired table's Pascal-Cased name:
        putStrLn $(stringE . pprint =<< reify ''Titles)
        putStrLn $(stringE . pprint =<< reify ''TitlesSearch)

    -David Bruck
-}
{-# LANGUAGE TemplateHaskell #-}
module Example where

import CrudGenerator
    ( generateCrud
    )

$(generateCrud "IMDB.db")
