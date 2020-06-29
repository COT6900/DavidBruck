{-# LANGUAGE TemplateHaskell #-}
module CrudGenerator where

import CrudTemplates
    ( generateCrud
    )

$(generateCrud "IMDB.db")

--someFunc :: IO ()
--someFunc = print (name, type')
--    where
--    Titles tconst titleType primaryTitle originalTitle startYear endYear runtimeMinutes genres =
--        Titles 1 "2" "3" "4"
--    --print $ curry4 add4 (2 :: Int) (4 :: Int) (6 :: Int) (8 :: Int)
