module Main where

import DatabaseHelpers
    ( withDatabase
    , databaseQuery
    )

import Control.Monad.Except
    ( runExceptT
    , throwError
    , ExceptT
    )
import Control.Exception
    ( toException
    , SomeException
    , Exception
    )

newtype InitializationException = InitializationException
    { error :: String
    } deriving (Show)

instance Exception InitializationException

main :: IO ()
main = do result <- runExceptT $ initializeDatabase `withDatabase` "IMDB.db"
          case result of
              Left err -> print $ "Error: " ++ show err
              Right _  -> putStrLn "Success"
    where
    initializeDatabase conn =
        do existing <- parameterlessQuery
                           ( "SELECT 1\n" ++
                             "FROM sqlite_master"
                           )
           if null existing
           then do parameterlessQuery
                       ( "CREATE TABLE titles (\n" ++
                         "tconst INTEGER PRIMARY KEY,\n" ++
                         "titleType TEXT NOT NULL,\n" ++
                         "primaryTitle TEXT NOT NULL,\n" ++
                         "originalTitle TEXT NOT NULL,\n" ++
                         "startYear INTEGER NOT NULL,\n" ++
                         "endYear INTEGER NULL,\n" ++
                         "runtimeMinutes INTEGER NULL,\n" ++
                         "genres TEXT\n" ++
                         ") WITHOUT ROWID"
                       )
                   parameterlessQuery
                       ( "CREATE VIRTUAL TABLE titles_search\n" ++
                         "USING FTS5(primaryTitle)"
                       )
           else throwError
                    $ toException
                    $ InitializationException "database already initialized"

        where
        parameterlessQuery sql = databaseQuery sql conn [] return
