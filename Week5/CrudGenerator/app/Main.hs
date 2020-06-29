module Main where

import CrudGenerator
import DatabaseHelpers
import Database.SQLite3
import Data.Either
import Control.Monad.Except
import Control.Exception
import Data.Text
--import CrudTemplates

main :: IO ()
main = do --let a = generateCrud "IMDB.db"
          limitOne <- runExceptT $ (`getTitlesSearch` 2) `withDatabase` "IMDB.db"
          case limitOne of
              Left err -> print err
              Right ttls -> forM_ ttls printTitlesSearch
          --print $ show (tconst, titleType, primaryTitle, originalTitle, startYear, endYear, runtimeMinutes, genres)
    where
    -- Titles tconst titleType primaryTitle originalTitle startYear endYear runtimeMinutes genres =
    --     Titles 1 "titleType2" "primaryTitle3" "originalTitle4" 2005 (Just 2006) (Just 2007) (Just "genres8")
    
    --printTitles (Titles tconst titleType primaryTitle originalTitle startYear endYear runtimeMinutes genres) = putStrLn $ "tconst: " ++ show (tconst, titleType, primaryTitle, originalTitle, startYear, endYear, runtimeMinutes, genres)
    printTitlesSearch (Titles_search primaryTitle rowid) = putStrLn $ "tconst: " ++ show (primaryTitle, rowid)

    --getTitles conn limit = databaseQuery "SELECT tconst, titleType, primaryTitle, originalTitle, startYear, endYear, runtimeMinutes, genres FROM titles ORDER BY tconst LIMIT ?" conn [SQLInteger (fromIntegral limit)] (\[SQLInteger tconst, SQLText titleType, SQLText primaryTitle, SQLText originalTitle, SQLInteger startYear, SQLNull, SQLNull, SQLNull] -> pure (Titles (fromIntegral tconst) (unpack titleType) (unpack primaryTitle) (unpack originalTitle) (fromIntegral startYear) Nothing Nothing Nothing))
    getTitlesSearch conn limit = databaseQuery "SELECT primaryTitle, rowid FROM titles_search WHERE primaryTitle MATCH 'title fake*' ORDER BY rank LIMIT ?" conn [SQLInteger (fromIntegral limit)] (\[primaryTitle, SQLInteger rowid] -> pure (Titles_search (case primaryTitle of SQLNull -> Nothing; SQLText primaryTitle -> Just $ unpack primaryTitle) (fromIntegral rowid)))
    --getTitles conn limit = databaseQuery "SELECT tconst, titleType, primaryTitle, originalTitle, startYear, endYear, runtimeMinutes, genres FROM titles ORDER BY tconst LIMIT ?" conn [SQLInteger (fromIntegral limit)] (\[SQLInteger tconst, SQLText titleType, SQLText primaryTitle, SQLText originalTitle, SQLInteger startYear, SQLNull, SQLNull, SQLNull] -> pure (Titles (fromIntegral tconst) (unpack titleType) (unpack primaryTitle) (unpack originalTitle) (fromIntegral startYear) Nothing Nothing Nothing))
    --    --print $ curry4 add4 (2 :: Int) (4 :: Int) (6 :: Int) (8 :: Int)
