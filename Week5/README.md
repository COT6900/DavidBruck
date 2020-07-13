### David Bruck COT6900 - Week 5, 6, 7

### Database helpers for SQLite

I added 3 public database helper functions:

* `withDatabase`: Opens a connection to a SQLite database and creates the database if it does not exist. While the connection is open, it calls a provided callback function with the open database connection. It always closes the connection once the callback function returns, and aggregates exceptions.
* `databaseQuery`: Takes a SQL string, open database connection (from `withDatabase`), parameters, and a transformer (to allow us to turn dynamically-typed arrays of column results into strongly-typed Haskell datatypes). Runs the parameterized SQL, transforms the untyped, and returns them typed.
* `getSchema`: Queries tables and virtual tables (e.g. FullText search tables) and includes their names, types, and columns (which themselves have names, types, nullability, primary key). This is used by the Template Haskell to automatically build strongly-typed Haskell datatypes and queries corresponding to the types and nullability for each field's original column.

DatabaseHelpers/src/DatabaseHelpers.hs :

```haskell
module DatabaseHelpers
    ( getSchema
    , withDatabase
    , databaseQuery
    , validMaybeText
    , validMaybeInt
    , SchemaColumn(..)
    , SchemaTable(..)
    , TableType(..)
    , SQLStringException(..)
    ) where

import Control.Exception
    ( bracket
    , toException
    , throwIO
    , SomeException
    , Exception
    )
import Data.Text
    ( pack
    , unpack
    , replace
    , Text
    )
import Data.List
    ( isSuffixOf
    )
import Data.Int
    ( Int64
    )
import Text.Printf
    ( printf
    )
import Database.SQLite3
    ( open
    , prepare
    , finalize
    , step
    , columns
    , bind
    , SQLData(..)
    , StepResult(..)
    )
import qualified Database.SQLite3.Direct
    ( close
    , Database
    )
import qualified Database.SQLite3.Bindings.Types
    ( Error
    )
import Control.Monad.Except
    ( runExceptT
    , lift
    , throwError
    , ExceptT
    )
import Control.Error.Util
    ( syncIO
    )
import qualified Data.HashSet
    ( fromList
    , member
    )

newtype SQLErrorException = SQLErrorException
    { enumError :: Database.SQLite3.Bindings.Types.Error
    } deriving (Show)

newtype SQLStringException = SQLStringException
    { stringError :: String
    } deriving (Show)

newtype AggregateException = AggregateException
    { errors :: [SomeException]
    } deriving (Show)

instance Exception SQLErrorException
instance Exception SQLStringException
instance Exception AggregateException

data TableType = Table | FullText | FullTextStorage
    deriving (Eq, Show)

data SchemaColumn = SchemaColumn
    { cid           :: Int64
    , name          :: Text
    , type'         :: Maybe Text
    , notnull       :: Int64
    , dflt_value    :: Maybe Text
    , pk            :: Int64
    }
    deriving (Show)

data SchemaTable = SchemaTable
    { tableName     :: Text
    , tableType     :: TableType
    , tableColumns  :: [SchemaColumn]
    }
    deriving (Show)

getSchema :: Database.SQLite3.Direct.Database ->
             ExceptT SomeException IO [SchemaTable]
getSchema conn =
    do tableNames <- parameterlessQuery
                         ( "SELECT name\n" ++
                           "FROM sqlite_master\n" ++
                           "WHERE type = 'table'"
                         )
                         nameTransformer
       let tableNamesAndTypes = appendTableType tableNames
       let nonFullTextStorageTables =
               filter
                   (\(tableName, tableType) ->
                       tableType /= FullTextStorage)
                   tableNamesAndTypes
       mapM getTableSchema nonFullTextStorageTables
    where
    parameterlessQuery sql = databaseQuery sql conn []

    getTableSchema :: (Text, TableType) -> ExceptT SomeException IO SchemaTable
    getTableSchema (tableName, tableType) =
        do columns <- parameterlessQuery
                          (pragmaTable
                              tableName)
                          schemaTransformer
           return $ SchemaTable
               tableName
               tableType
               (if tableType == FullText
                then columns ++
                         [fullTextRowID
                             $ length columns]
                else columns)

    fullTextRowID colsLength =
        SchemaColumn
        { cid           = fromIntegral colsLength
        , name          = pack "rowid"
        , type'         = Just $ pack "INTEGER"
        , notnull       = 1
        , dflt_value    = Nothing
        , pk            = 1
        }
    appendTableType tableNames =
        map
            (\tableName -> (tableName, getTableType $ unpack tableName))
            tableNames
        where
        getTableType tableName
            | Just endingLength <- matchFullTextStorageEnding
                                       tableName
                                       fullTextStorageEndings
            , isFullTextTable
                  $ take (length tableName - endingLength) tableName =
                      FullTextStorage
            | isFullTextTable tableName = FullText
            | otherwise                 = Table
        isFullTextTable tableName =
            all
                (\ending -> Data.HashSet.member
                                (pack $ tableName ++ ending)
                                tableNamesSet)
                fullTextStorageEndings

        matchFullTextStorageEnding :: String -> [String] -> Maybe Int
        matchFullTextStorageEnding _ []             = Nothing
        matchFullTextStorageEnding tableName (x:xs) =
            if x `isSuffixOf` tableName
            then Just $ length x
            else matchFullTextStorageEnding tableName xs
                                                               
        fullTextStorageEndings  = [ "_data"
                                  , "_idx"
                                  , "_content"
                                  , "_docsize"
                                  , "_config"
                                  ]
        tableNamesSet           = Data.HashSet.fromList tableNames

    pragmaTable name = printf
                           "pragma table_info(\"%s\")"
                           escapedName
        where
        escapedName = unpack $ replace oneQuote twoQuotes name
        oneQuote = pack "\""
        twoQuotes = pack "\"\""

    nameTransformer [SQLText name]  = return name
    nameTransformer _                   =
        throwIO $ SQLStringException "expecting schema columns (Text NOT NULL)"

    schemaTransformer
        [ SQLInteger cid
        , SQLText name
        , type'
        , SQLInteger notnull
        , dflt_value
        , SQLInteger pk
        ]
        | True <- validMaybeText type'
        , True <- validMaybeText dflt_value =
            return $ SchemaColumn
                         cid
                         name
                         (maybeText type')
                         notnull
                         (maybeText dflt_value)
                         pk
        where
        maybeText SQLNull               = Nothing
        maybeText (SQLText value)       = Just value
    schemaTransformer _ =
        throwIO $ SQLStringException
            ( "expecting schema columns (Int NOT NULL, Text NOT NULL, " ++
              "Text NULL, Int NOT NULL, Text NULL, Int NOT NULL)"
            )

validMaybeText maybeText
    | SQLNull   <- maybeText    = True
    | SQLText _ <- maybeText    = True
    | otherwise                 = False

validMaybeInt maybeInt
    | SQLNull       <- maybeInt = True
    | SQLInteger _  <- maybeInt = True
    | otherwise                 = False

withDatabase :: (Database.SQLite3.Direct.Database ->
                    ExceptT SomeException IO a) ->
                String ->
                ExceptT SomeException IO a
withDatabase operations name =
    do conn     <- syncIO $ open $ pack name
       result   <- syncIO $ runExceptT $ operations conn
       case result of
           Left err         -> do closed <- lift
                                                $ runExceptT
                                                $ closeDatabase conn
                                  case closed of
                                      {-
                                          An error occurred on database
                                          operations and also closing the
                                          connection; aggregate both
                                      -}
                                      Left err' -> throwError (toException
                                                       $ AggregateException
                                                           [err, err'])
                                      Right _   -> throwError err
           Right result'    -> do closeDatabase conn
                                  return result'

    where
    closeDatabase :: Database.SQLite3.Direct.Database ->
                     ExceptT SomeException IO ()
    closeDatabase conn =
        do closed <- syncIO $ Database.SQLite3.Direct.close conn
           case closed of
               Left err -> throwError $ toException $ SQLErrorException err
               Right _  -> pure ()

databaseQuery :: String ->
                 Database.SQLite3.Direct.Database ->
                 [SQLData] ->
                 ([SQLData] -> IO a) ->
                 ExceptT SomeException IO [a]
databaseQuery sql conn params transform =
    syncIO $ bracket
                 (do stmt <- prepare conn $ pack sql
                     if null params
                     then pure ()
                     else bind stmt params
                     return stmt)
                 finalize
                 readRows

    where
    readRows stmt =
        do result <- step stmt
           case result of
               Row  -> do x     <- columns stmt >>= transform
                          xs    <- readRows stmt
                          return (x : xs)
               Done -> pure []
```



### Initialize Database

Creates first table in the database for IMDB "Titles" (e.g. movie listings).

To use, change directory to Week5 folder in a terminal, then run command:

* `cabal v2-run InitializeDatabase`

This will create an IMDB.db file in the current directory. It is used when you run the other applications.

There is currently no code written to populate data into this database, so you can create test records by loading up a SQLite commandline executable like sqlite3.exe and running the following SQL:

```sql
INSERT INTO titles (tconst, titleType, primaryTitle, originalTitle, startYear, endYear, runtimeMinutes, genres)
VALUES (1, 'fake', 'fake title', 'fake title', 2020, NULL, NULL, NULL);

INSERT INTO titles (tconst, titleType, primaryTitle, originalTitle, startYear, endYear, runtimeMinutes, genres)
VALUES (2, 'fake', 'fake titles', 'fake titles', 2020, NULL, NULL, NULL);

INSERT INTO titles_search (rowid, primaryTitle) SELECT tconst, primaryTitle FROM titles;
```



Also, created library function used by Setup.hs in both PopulateDatabase and MovieSearchSite projects in order to help ensure InitializeDatabase is run at least once before CrudGenerator can be used to use the database schema to run TemplateHaskell and build CRUD operations.

InitializeDatabase/src/InitializeDatabase.hs

```haskell
module InitializeDatabase
    ( initializeDatabase
    ) where

import DatabaseHelpers
    ( databaseQuery
    )
import System.Process
    ( createProcess
    , waitForProcess
    , proc
    , StdStream(..)
    , CreateProcess(..)
    )
import Control.Monad
    ( when
    )
import Control.Error.Util
    ( syncIO
    )
import System.IO
    ( stdout
    , stderr
    , hFlush
    , hGetContents
    , hPutStrLn
    )
import System.Exit
    ( ExitCode(..)
    )

initializeDatabase conn =
    do existing <- databaseQuery
                       ( "SELECT 1\n" ++
                         "FROM sqlite_master"
                       )
                       conn
                       []
                       return
       syncIO $ when (null existing) runInitialize

    where
    runInitialize =
        do (_, hOut, hErr, process) <- createProcess
                                           (proc
                                               "cabal"
                                               [ "v2-run"
                                               , "../InitializeDatabase"
                                               ])
                                           { std_out = CreatePipe
                                           , std_err = CreatePipe
                                           }
           exitCode <- waitForProcess process
           out      <- getContents hOut "Out"
           err      <- getContents hErr "Err"
           case exitCode of
               ExitFailure code -> error $
                                       "InitializeDatabase " ++
                                       "failed with exit code " ++
                                       show code ++ "\nOutput:\n" ++
                                       out ++ err
               _                -> do putStrLn out
                                      hFlush stdout
                                      hPutStrLn stderr err
                                      hFlush stderr

        where
        getContents h purpose =
            case h of
                Just h  -> hGetContents h
                _       -> return $ "Error: " ++ purpose ++ " handle Nothing"
```



The executable itself, InitializeDatabase/app/Main.hs :

```haskell
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
              Left err -> Prelude.error $ "Error: " ++ show err
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
                         "startYear INTEGER NULL,\n" ++
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
```



### Template Haskell CRUD Generator

Template Haskell `$(generateCrud "<<path_to_SQLite_db_file>>")` will generate datatypes for all the tables and virtual tables in the SQLite database file specified by its string parameter.

Generated types:

* Datatypes `<<TableName>>` with fields `<<tableName>>_<<columnName>>` for each column. A special additional column is created for `rowid` only for FullText type tables.
* For both normal and FullText tables, added inserts `insert<<TableName>>` taking a record as the table's generated datatype to insert into the database.
* For normal tables, queries `get<<TableName>>` taking a `limit` parameter for the number of records to return, in the order of the primary key ascending.
* Also for normal tables, queries `get<<TableName>>By<<ColumnName>>` taking an array of values (`query` parameter) to find in a single column as well as the same `limit` parameter.
  * If the query is empty, no records are returned.
  * If the query has exactly one non-`Nothing` value, it produces a SQL `WHERE <<column_name>> = ?`.
  * If the query has exactly one value and it's `Nothing`, it produces a SQL `WHERE <<column_name>> IS NULL`.
  * And finally, if the query has more than one value, it produces a SQL `WHERE <<column_name>> IN (?, ?, ..., ?)` for all queried inputs.
* For FullText tables, queries `get<<TableName>>By<<ColumnName>>` for the column to search. Takes a `query` parameter which will be split by words, having each suffixed with special `'*'` wildcard character. Builds SQL like ``WHERE <<column_name>> MATCH ?` and adds `ORDER BY rank` which is special for FullText tables to return results in order from best to worst match.

![TemplateHaskellExampleOutput](TemplateHaskellExampleOutput.png)



CrudGenerator/src/CrudGenerator.hs :

```haskell
{-# LANGUAGE TemplateHaskell #-}
module CrudGenerator
    ( generateCrud
    ) where

import DatabaseHelpers
    ( withDatabase
    , getSchema
    , validMaybeText
    , validMaybeInt
    , databaseQuery
    , SchemaColumn(..)
    , SchemaTable(..)
    , TableType(..)
    , SQLStringException(..)
    )
import Data.Text
    ( unpack
    , pack
    , null
    )
import Data.Char
    ( toUpper
    , toLower
    )
import Data.List
    ( intercalate
    )
import Data.Maybe
    ( fromJust
    )
import Control.Monad.Except
    ( runExceptT
    , ExceptT
    )
import Control.Exception
    ( toException
    , throw
    , throwIO
    , SomeException
    , Exception
    )
import Database.SQLite3
    ( SQLData(..)
    )
import Language.Haskell.TH
    ( runIO
    , recC
    , dataD
    , derivClause
    , mkName
    , varBangType
    , bangType
    , bang
    , conT
    , conE
    , conP
    , sigD
    , funD
    , varP
    , varE
    , varT
    , litT
    , litE
    , clause
    , normalB
    , guardedB
    , listP
    , listE
    , appE
    , patGE
    , noBindS
    , stringE
    , wildP
    , SourceUnpackedness(..)
    , SourceStrictness(..)
    )

newtype UnhandledSchemaException = UnhandledSchemaException
    { error :: String
    } deriving (Show)

instance Exception UnhandledSchemaException

generateCrud filePath = runIO getDatabaseSchema >>= crudFromSchema

    where
    getDatabaseSchema =
        do result <- runExceptT $ getSchema `withDatabase` filePath
           case result of
               Left err     -> throw err
               Right schema -> return schema

    crudFromSchema []       = throw $ toException
                                  $ UnhandledSchemaException
                                        "uninitialized database"
    crudFromSchema tables   =
        do dataTypes        <- generateDataTypes tables
           transformers     <- generateTransformers tables
           queries          <- generateQueries tables
           inserts          <- generateInserts tables
           return $ dataTypes ++ transformers ++ queries ++ inserts

    generateDataTypes []        = pure []
    generateDataTypes (x:xs)    =
        do columns <- crudFields $ tableColumns x
           fields <- recC capitalizedName (map return columns)
           dataType <- dataD
                           (pure [])
                           capitalizedName
                           []
                           Nothing
                           [pure fields]
                           [derivClause Nothing [[t| Show |]]]
           rest <- generateDataTypes xs
           return (dataType : rest)

        where
        unpackedTableName   = unpack $ tableName x
        capitalizedName     = mkName $ capitalize unpackedTableName
        lowercasedName      = lowercase unpackedTableName

        crudFields []     = pure []
        crudFields (x:xs) =
            do field <- varBangType
                            (mkName (lowercasedName ++ "_" ++ unpack (name x)))
                            (bangType
                                (bang
                                    (pure NoSourceUnpackedness)
                                    (pure NoSourceStrictness))
                                (columnType x))
               rest <- crudFields xs
               return (field : rest)

            where
            columnType column =
                columnTypeTextOrInt
                    (makeType "String" notnullInt)
                    (makeType "Int" notnullInt)
                    column

                where
                notnullInt = notnull column

        makeType haskellType notnull =
            if notnull == 1
            then conT $ mkName haskellType
            else [t| Maybe $(conT $ mkName haskellType) |]

    generateTransformers []      = pure []
    generateTransformers (x:xs)  =
        do transformerType  <- sigD
                                   transformerName
                                   [t| [SQLData] -> IO $(conT capitalizedName) |]
           transformer      <- funD
                                   transformerName
                                   [ if all ((==1) . notnull) $ tableColumns x
                                     then simpleTransformer
                                     else maybeTransformer
                                   , transformerError
                                   ]
           rest             <- generateTransformers xs
           return (transformerType : transformer : rest)

        where
        (unpackedTableName, transformerName, columns) = commonTableProps x
        capitalizedName = mkName $ capitalize unpackedTableName

        transformerArgs []      = []
        transformerArgs (x:xs)  =
            (if notnull x == 1
             then columnTypeTextOrInt textArg intArg x
             else argName)
            : transformerArgs xs

            where
            argName         = varP $ makeColumnName x
            textArg         = [p| SQLText $(argName) |]
            intArg          = [p| SQLInteger $(argName) |]

        transformerError =
            clause
                [varP $ mkName "_"]
                (normalB [e| throwIO $ SQLStringException
                                 $(stringE
                                     $ "for table " ++ unpackedTableName ++
                                       ", expecting schema columns (" ++
                                       intercalate ", " (map
                                           (\column ->
                                               maybe
                                                   "TEXT"
                                                   unpack
                                                   (notEmptyColumnType
                                                       $ type' column) ++
                                               (if notnull column == 1
                                                then " NOT"
                                                else "") ++
                                               " NULL")
                                           columns) ++
                                       ")"
                                 )
                         |])
                []

            where
            notEmptyColumnType columnType
                | Just txt <- columnType
                , Data.Text.null txt    = Nothing
                | otherwise             = columnType

        simpleTransformer =
            clause
                [listP $ transformerArgs columns]
                (normalB $ transform columns)
                []

        maybeTransformer =
            clause
                [listP $ transformerArgs columns]
                (guardedB
                    [patGE
                        (maybeTransformerGuards columns)
                        [e| pure $(transform columns) |]])
                []

        transform columns =
            do let constructor = conE capitalizedName
               transformImpl constructor columns

            where
            transformImpl earlier []        = earlier
            transformImpl earlier (x:xs)    =
                do let appended = appE earlier transformColumn
                   transformImpl appended xs

                where
                columnName = makeColumnName x
                transformColumn
                    | notnull x == 1 =
                        columnTypeTextOrInt unpackText fromIntegralInt64 x
                    | otherwise =
                        [e| case $(varE columnName) of
                                SQLNull -> Nothing
                                $(columnTypeTextOrInt
                                    (conP
                                        'SQLText
                                        [[p| $(varP columnName) |]])
                                    (conP
                                        'SQLInteger
                                        [[p| $(varP columnName) |]])
                                    x) -> Just
                                        $(columnTypeTextOrInt
                                            unpackText
                                            fromIntegralInt64
                                            x) |]

                    where
                    unpackText          = [e| unpack $(varE columnName) |]
                    fromIntegralInt64   = [e| fromIntegral $(varE columnName) |]

        maybeTransformerGuards []       = []
        maybeTransformerGuards (x:xs)   =
            do let columnName = makeColumnName x
               (if notnull x == 1
                then id
                else (:) $ noBindS $ columnTypeTextOrInt
                             [e| validMaybeText $(varE columnName) |]
                             [e| validMaybeInt $(varE columnName) |]
                             x)
                   $ maybeTransformerGuards xs

    generateQueries []      =
        do emptySig <- sigD
                           getEmpty
                           [t| ExceptT SomeException IO [$(varT $ mkName "a")] |]
           getEmpty <- funD
                           getEmpty
                           [clause
                                []
                                (normalB [e| pure [] |])
                                []]
           return [emptySig, getEmpty]

    generateQueries (x:xs)  =
        do queries  <- case tableType x of
                           Table       -> generateTableQueries columns
                           FullText    -> generateFullTextQueries columns
                           _           -> throw $ toException
                                              $ UnhandledSchemaException
                                              $ "unhandled SQL table type "
                                                    ++ show x
           rest     <- generateQueries xs
           return $ queries ++ rest

        where
        (unpackedTableName, transformerName, columns) = commonTableProps x
        getName         = "get" ++ capitalize unpackedTableName
        selectColumns   = intercalate ", "
                              $ map
                                    (escapeSQLiteName . unpack . name)
                                    columns
        baseSQL         = "SELECT " ++ selectColumns ++
                              " FROM " ++
                              escapeSQLiteName unpackedTableName
        orderSQL        =
            let pkColumns = filter ((==1) . pk) columns
            in " ORDER BY " ++
                   (if Prelude.null pkColumns
                    then "rowid"
                    else intercalate ", "
                             $ map (escapeSQLiteName . unpack . name) pkColumns)

        generateTableQueries [] =
            do query <- query
               pure [query]

            where
            sql     = baseSQL ++ orderSQL ++ limitSQL
            query   =
                do let (_, limitName, connName) = commonQueryNames
                   funD
                       (mkName getName)
                       [clause
                            [ varP limitName
                            , varP connName
                            ]
                            (normalB [e| databaseQuery
                                             $(stringE sql)
                                             $(varE connName)
                                             [SQLInteger $ fromInteger
                                                  $(varE limitName)]
                                             $(varE transformerName) |])
                            []]

        generateTableQueries (x:xs) =
            do query    <- query
               rest     <- generateTableQueries xs
               return $ query : rest

            where
            query   =
                funD
                    (mkName $ getName ++
                        "By" ++ capitalize columnName)
                    [ clause
                         [ [p| [] |]
                         , wildP
                         , wildP
                         ]
                         (normalB
                             $ varE getEmpty)
                         []
                    , clause
                         [ [p| [$(varP queryName)] |]
                         , varP limitName
                         , varP connName
                         ]
                         (normalB
                              $ if notnull x == 1
                                then equalsQuery
                                else [e| case $(varE queryName) of
                                             Just $(varP queryName) ->
                                                  $(equalsQuery)
                                             Nothing -> $(nullQuery) |])
                         []
                    , clause
                         [ varP queryName
                         , varP limitName
                         , varP connName
                         ]
                         (normalB inQuery)
                         []
                    ]

            nullQuery   =
                [e| databaseQuery
                        $(stringE $ baseSQL ++ whereSQL ++
                              " IS NULL" ++ orderSQL ++ limitSQL)
                        $(varE connName)
                        [SQLInteger $ fromInteger
                             $(varE limitName)]
                        $(varE transformerName) |]

            equalsQuery =
                [e| databaseQuery
                        $(stringE $ baseSQL ++ whereSQL ++
                              " = ?" ++ orderSQL ++ limitSQL)
                        $(varE connName)
                        [ $(columnTypeTextOrInt
                                [e| SQLText $ pack $(varE queryName) |]
                                [e| SQLInteger $ fromInteger
                                        $(varE queryName) |]
                                x)
                        , SQLInteger $ fromInteger
                              $(varE limitName)
                        ]
                        $(varE transformerName) |]

            inQuery     =
                [e| databaseQuery
                        ($(stringE $ baseSQL ++ whereSQL ++ " IN (") ++
                             intercalate ", "
                                 (replicate
                                     (length $(varE queryName))
                                     "?") ++
                             $(stringE $ ")" ++ orderSQL ++ limitSQL))
                        $(varE connName)
                        (foldr
                            (\ $(varP lName) r ->
                                $(if notnull x == 1
                                  then toTextOrInt
                                  else [e| case $(varE lName) of
                                               Just $(varP lName) ->
                                                   $(toTextOrInt)
                                               _ -> SQLNull |])
                                : r)
                            [SQLInteger $ fromInteger
                              $(varE limitName)]
                            $(varE queryName))
                        $(varE transformerName) |]

            toTextOrInt                         =
                columnTypeTextOrInt
                    [e| SQLText $ pack $(varE lName) |]
                    [e| SQLInteger $ fromInteger $(varE lName) |]
                    x
            lName                               = mkName "l"
            columnName                          = unpack $ name x
            whereSQL                            =
                " WHERE " ++ escapeSQLiteName columnName
            (queryName, limitName, connName)    = commonQueryNames

        generateFullTextQueries []      = pure []
        generateFullTextQueries (x:xs)  =
            if columnName == "rowid"
            then generateFullTextQueries xs
            else do query    <- funD
                                    (mkName $ getName ++
                                        "By" ++ capitalize columnName)
                                    [query]
                    rest     <- generateFullTextQueries xs
                    return $ query : rest

            where
            columnName  = unpack $ name x
            query       =
                do let (queryName, limitName, connName) = commonQueryNames
                   clause
                       [ varP queryName
                       , varP limitName
                       , varP connName
                       ]
                       (normalB [e| databaseQuery
                                        $(stringE sql)
                                        $(varE connName)
                                        [ SQLText $ pack $ unwords
                                              $ map
                                                    (++"*")
                                                    (words
                                                        $(varE queryName))
                                        , SQLInteger $ fromInteger
                                              $(varE limitName)]
                                        $(varE transformerName) |])
                       []
            sql         =
                baseSQL ++ " WHERE " ++
                escapeSQLiteName columnName ++
                " MATCH ? ORDER BY rank" ++ limitSQL

    generateInserts []      = pure []
    generateInserts (x:xs)  =
        do insert   <- generateInsert
           rest     <- generateInserts xs
           return $ insert : rest

        where
        (unpackedTableName, _, columns) = commonTableProps x
        lowercasedName  = lowercase unpackedTableName
        insertName      = "insert" ++ capitalize unpackedTableName
        insertColumns   = intercalate ", "
                              $ map
                                    (escapeSQLiteName . unpack . name)
                                    columns
        insertSQL       = "INSERT INTO " ++
                              escapeSQLiteName unpackedTableName ++
                              " (" ++ insertColumns ++ ") VALUES (" ++
                              intercalate ", " (replicate
                                                   (length columns)
                                                   "?") ++
                              ")"
        param column    =
            do let columnName       = unpack (name column)
               let lowercasedColumn = mkName $ lowercase columnName
               let valueName        = mkName $ lowercasedName ++ "_" ++
                                          columnName
               let getColumn        = [e| $(varE valueName)
                                              $(varE $ mkName lowercasedName) |]
               columnTypeTextOrInt
                   (case notnull column of
                        1  -> [e| SQLText $ pack $(getColumn) |]
                        _  -> [e| case $(getColumn) of
                                      Just $(varP lowercasedColumn) ->
                                          SQLText $ pack $(varE lowercasedColumn)
                                      Nothing -> SQLNull |])
                   (case notnull column of
                        1  -> [e| SQLInteger $ fromIntegral $(getColumn) |]
                        _  -> [e| case $(getColumn) of
                                      Just $(varP lowercasedColumn) ->
                                          SQLInteger $ fromIntegral
                                              $(varE lowercasedColumn)
                                      Nothing -> SQLNull |])
                   column

        generateInsert =
            do let (_, _, connName) = commonQueryNames
               let paramNames       = map makeColumnName columns
               funD
                   (mkName insertName)
                   [clause
                        [ varP $ mkName lowercasedName
                        , varP connName
                        ]
                        (normalB [e| databaseQuery
                                         $(stringE insertSQL)
                                         $(varE connName)
                                         $(listE $ map param columns)
                                         (\_ -> pure ()) |])
                        []]

    limitSQL                = " LIMIT ?"
    lowerCaseColumn         = lowercase . unpack . name
    makeColumnName          = mkName . lowerCaseColumn
    getEmpty                = mkName "getEmpty"
    commonQueryNames        = (mkName "query", mkName "limit", mkName "conn")
    commonTableProps table  = (unpackedTableName, transformerName, columns)

        where
        unpackedTableName   = unpack $ tableName table
        transformerName     = mkName $ lowercase unpackedTableName ++ "Transformer"
        columns             = tableColumns table

    columnTypeTextOrInt onText onInteger column =
        case columnType of
            Just txt    -> case unpack txt of
                           ""           -> onText
                           "TEXT"       -> onText
                           "INTEGER"    -> onInteger
                           _            -> unhandledColumnType
            _           -> onText

        where
        columnType          = type' column
        unhandledColumnType = throw $ toException
                                  $ UnhandledSchemaException
                                  $ "unhandled SQL column type " ++
                                        unpack (fromJust columnType)

    capitalize []               = []
    capitalize (x:xs)           = toUpper x : upperCaseWords xs

    lowercase []                = []
    lowercase (x:xs)            = toLower x : upperCaseWords xs

    upperCaseWords []           = []
    upperCaseWords ('_':x:xs)   = toUpper x : upperCaseWords xs
    upperCaseWords (x:xs)       = x : upperCaseWords xs

    escapeSQLiteName name       = '"' : escapeSQLiteNameImpl name

        where
        escapeSQLiteNameImpl []     = "\""
        escapeSQLiteNameImpl (x:xs) = (if x == '"'
                                       then ('"':) . ('"':)
                                       else (x:))
                                       $ escapeSQLiteNameImpl xs
```



CrudGenerator/Example.hs :

```haskell
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
```



### Parse IMDB's Database of Titles Into SQLite

**Prerequisite:** in order to use CrudGenerator for Template Haskell, the database must already exist with the expected Schema; therefore, I created a Cabal custom build which also runs InitializeDatabase for the current directory to create IMDB.db in the expected format if it does not already exist.

PopulateDatabase/Setup.hs :

```haskell
import Distribution.Simple
    ( defaultMainWithHooks
    , simpleUserHooks
    , UserHooks(..)
    )
import DatabaseHelpers
    ( withDatabase
    )
import InitializeDatabase
    ( initializeDatabase
    )
import Control.Monad.Except
    ( runExceptT
    )

main = defaultMainWithHooks buildHooked

    where
    buildHooked = simpleUserHooks {buildHook = initializeDatabaseFirst}

    initializeDatabaseFirst pdesc lbinfo uhooks bflags =
        do result <- runExceptT $ initializeDatabase `withDatabase` "IMDB.db"
           case result of
               Left err -> error $ "Error: " ++ show err
               Right _  -> buildHook simpleUserHooks pdesc lbinfo uhooks bflags
```



PopulateDatabase-exe is able to download and unzip IMDB database file "title.basics.tsv.gz", but decoding tab-separated values, or TSV, with cassava is difficult because there are no decode options to prevent it from recognizing a double quote (`"`) character as an escaped field. Example console output:

```shell
Downloading https://datasets.imdbws.com/title.basics.tsv.gz ...
Unzipping and decoding ...
PopulateDatabase-exe: Error: DecodeException {decodeError = "parse error (Failed reading: conversion error: cannot unpack array of length 3 into a 3-tuple. Input record: [\"tt0033122\",\"movie\",\"Swing it\"]) at  magistern\t\"Swing it\" magistern\t0\t1940\t\\N\t92\tComedy,Music\ntt0033123\tshort\tSwinging the Lambeth Walk\t (truncated)"}
CallStack (from HasCallStack):
  error, called at app\Main.hs:85:27 in main:Main
```



To workaround the issue parsing as a TSV, we created a routine `tsvToCsv` which works on chunks of "Lazy Bytestring" to create parseable comma-separated values, or CSV. Also, to prevent memory from growing, we immediately save the CSV to disk, before opening it for read again to parse into records.

PopulateDatabase/app/Main.hs :

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import DatabaseHelpers
    ( withDatabase
    , databaseQuery
    , SQLStringException(..)
    )
import Control.Monad
    ( when
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
import Control.Error.Util
    ( syncIO
    )
import Network.HTTP.Simple
    ( parseRequest
    , httpBS
    , getResponseBody
    )
import Codec.Compression.GZip
    ( decompress
    )
import Data.Text.Lazy.Encoding
    ( decodeUtf8
    )
import Data.Csv
    ( decodeByName
    , Header
    , FromNamedRecord
    )
import Data.Either
    ( either
    )
import Data.Char
    ( ord
    )
import Data.Vector
    ( head
    , forM_
    , Vector
    )
import Data.Text
    ( unpack
    , Text
    )
import Data.ByteString
    ( pack
    , unpack
    , empty
    , writeFile
    , appendFile
    )
import Data.ByteString.Lazy
    ( fromStrict
    , toChunks
    , fromChunks
    , readFile
    )
import Data.Function
    ( (&)
    )
import System.Directory
    ( doesFileExist
    )
import GHC.Generics
    ( Generic
    )
import Data.Word
    ( Word8(..)
    )
import CrudGenerator
    ( generateCrud
    )
import Text.Read
    ( readEither
    )

$(generateCrud "IMDB.db")

newtype DecodeException = DecodeException
    { decodeError :: String
    } deriving (Show)

instance Exception DecodeException

data Title = Title
    { tconst            :: !Text -- Combination: "tt" ++ readable Int
    , titleType         :: !Text
    , primaryTitle      :: !Text
    , originalTitle     :: !Text
    , isAdult           :: !Int
    , startYear         :: !Text -- Could be Int, but could be "\N"othing
    , endYear           :: !Text -- Could be Int, but could be "\N"othing
    , runtimeMinutes    :: !Text -- Could be Int, but could be "\N"othing
    , genres            :: !Text -- Could be "\N"othing
    }
    deriving (Generic, Show)

instance FromNamedRecord Title

main = do result <- runExceptT $ initializeDatabase `withDatabase` "IMDB.db"
          case result of
              Left err -> Prelude.error $ "Error: " ++ show err
              Right _  -> putStrLn "Success"

    where
    initializeDatabase conn =
        do let titleBasicsURL   = "https://datasets.imdbws.com/title.basics.tsv.gz"
           let convertedCsvPath = "title.basics.csv"

           hasSchema    <- databaseQuery
                               ( "SELECT 1\n" ++
                                 "FROM sqlite_master"
                               )
                               conn
                               []
                               return
           hasTitles    <-
               if null hasSchema
               then (throwError . toException . SQLStringException)
                        "database lacks schema"
               else databaseQuery
                        ( "SELECT 1\n" ++
                          "FROM titles"
                        )
                        conn
                        []
                        return

           fileExists   <-
               if null hasTitles
               then syncIO $ doesFileExist convertedCsvPath
               else (throwError . toException . SQLStringException)
                        "database already has titles"

           if fileExists
           then syncIO $ putStrLn $ "Using existing " ++ convertedCsvPath
           else do syncIO $ putStrLn $ "Downloading " ++ titleBasicsURL ++ " ..."
                   downloaded   <- syncIO $ parseRequest titleBasicsURL >>= httpBS

                   syncIO $ putStrLn "Unzipping, converting to csv, saving ..."
                   syncIO $ putStrLn $ "Watch size of file for progress: " ++
                                           convertedCsvPath
                   syncIO $ putStrLn "Expected size: > 1/2 GB"
                   syncIO $ writeCsv convertedCsvPath
                                $ getResponseBody downloaded
                                      & fromStrict
                                      & decompress
                                      & tsvToCsv
                   syncIO $ putStrLn "Conversion to csv complete, continuing ..."

           syncIO $ putStrLn "Parsing csv to records ..."
           csv <- syncIO $ Data.ByteString.Lazy.readFile convertedCsvPath

           vector <- either
                         (throwError . toException . DecodeException)
                         (return . snd)
                         (decodeByName csv
                             :: Either String (Header, Vector Title))

           syncIO $ putStrLn "Adding records to database ..."
           forM_ vector insertRecord

        where
        insertRecord title =
            do parsedTConst     <- parseTConst $ Data.Text.unpack $ tconst title
               parsedStartYear  <- parseTitleMaybe $ startYear title
               parsedEndYear    <- parseTitleMaybe $ endYear title
               parsedRunMinutes <- parseTitleMaybe $ runtimeMinutes title
               parsedGenres     <- parseTitleMaybe $ genres title

               let currentTitleType = Data.Text.unpack $ titleType title
               let currentPrimTitle = Data.Text.unpack $ primaryTitle title
               let currentOrigTitle = Data.Text.unpack $ originalTitle title
               if isAdult title == 0 -- Don't want anything inappropriate
               then do insertTitles
                           Titles
                           { titles_tconst              = parsedTConst
                           , titles_titleType           = currentTitleType
                           , titles_primaryTitle        = currentPrimTitle
                           , titles_originalTitle       = currentOrigTitle
                           , titles_startYear           = parsedStartYear
                           , titles_endYear             = parsedEndYear
                           , titles_runtimeMinutes      = parsedRunMinutes
                           , titles_genres              = parsedGenres
                           }
                           conn
                       insertTitlesSearch
                           TitlesSearch
                           { titlesSearch_primaryTitle  = Just currentPrimTitle
                           , titlesSearch_rowid         = parsedTConst
                           }
                           conn
               else pure [()]

        parseTConst :: String -> ExceptT SomeException IO Int
        parseTConst ('t':'t':tconst)    =
            case readEither tconst of
                Left err    -> throwError $ toException $ DecodeException
                                   $ "when reading title tconst after " ++
                                         "\"tt\": " ++ tconst ++ ", error: " ++
                                         err
                Right value -> return value
        parseTConst tconst              =
            throwError $ toException $ DecodeException
                $ "expected title tconst to start with \"tt\": " ++
                      if null tconst
                      then "(null)"
                      else tconst

        parseTitleMaybe :: (Read a) => Text -> ExceptT SomeException IO (Maybe a)
        parseTitleMaybe value =
            let unparsed = Data.Text.unpack value
            in if unparsed == "\\N"
               then pure Nothing
               else case readEither unparsed of
                        Left err    -> throwError $ toException $ DecodeException
                                           $ "error reading: " ++ err
                        Right value -> return $ Just value

        writeCsv = writeCsvImpl True

            where
            writeCsvImpl replaceFile path []        =
                when replaceFile $ Data.ByteString.writeFile path empty
            writeCsvImpl replaceFile path (x:xs)    =
                do writeOrAppend path x
                   writeCsvImpl False path xs
                                                   
                where
                writeOrAppend = if replaceFile
                                then Data.ByteString.writeFile
                                else Data.ByteString.appendFile

        tab, comma, doubleQuote, newline, cr :: Word8
        tab         = fromIntegral (ord '\t')
        comma       = fromIntegral (ord ',')
        doubleQuote = fromIntegral (ord '"')
        newline     = fromIntegral (ord '\n')
        cr          = fromIntegral (ord '\r')

        tsvToCsv = tsvToCsvImpl [] 0 [] . toChunks

            where
            chunkSize = 1024
            tsvToCsvImpl rebuilt rebuiltLength carryOver chunks =
                let (nextCarryOver, nextChunks, field, needsEscape, separator) =
                        untilNextSeparator carryOver chunks
                    quoted          = if needsEscape
                                      then doubleQuote : quoteValue field
                                      else field
                    toAppend        = case separator of
                                           Nothing          -> quoted
                                           Just separator   ->
                                               quoted ++
                                                   if separator == tab
                                                   then [comma]
                                                   else [separator]
                    toAppendLength  = length toAppend
                    combinedLength  = rebuiltLength + toAppendLength
                    isFilled        = combinedLength >= chunkSize
                    (resultChunk, newRebuilt, newRebuiltLength) =
                        if isFilled
                        then let (lastEnd, newStart) =
                                     splitAt
                                         (chunkSize - rebuiltLength)
                                         toAppend
                             in ( Just $ rebuilt ++ lastEnd
                                , newStart
                                , combinedLength - chunkSize)
                        else (Nothing, rebuilt ++ toAppend, combinedLength)
                in case separator of
                       Just separator   -> let next = tsvToCsvImpl
                                                          newRebuilt
                                                          newRebuiltLength
                                                          nextCarryOver
                                                          nextChunks
                                           in case resultChunk of
                                                  Nothing      -> next
                                                  Just filled  ->
                                                      pack filled : next
                       Nothing          -> case resultChunk of
                                               Nothing      -> [pack newRebuilt]
                                               Just filled  ->
                                                   pack filled : [pack newRebuilt]

            untilNextSeparator []       []      = ([], [], [], False, Nothing)
            untilNextSeparator []       (x:xs)  = untilNextSeparator
                                                      (Data.ByteString.unpack x)
                                                      xs
            untilNextSeparator (x:xs)   chunks
                -- Strip carriage returns
                | x == cr                   = untilNextSeparator xs chunks
                | x `elem` [newline, tab]   = (xs, chunks, [], False, Just x)
                | otherwise =
                    ( nextCarryOver
                    , nextChunks
                    , x : field
                    , needsEscape || x `elem` [comma, doubleQuote]
                    , separator
                    )
                    where
                    ( nextCarryOver
                        , nextChunks
                        , field
                        , needsEscape
                        , separator) = untilNextSeparator xs chunks

            quoteValue []           = [doubleQuote]
            quoteValue (x:xs)
                | x == doubleQuote  = doubleQuote : doubleQuote : quoteValue xs
                | otherwise         = x : quoteValue xs
```



### Movie Search Site (HTTP Server hosting IMDB.db)

Work-in-progress, able to route HTTP "GET" requests. Relying on Haskell's own function pattern matching, any attempt to route which fails throws `PatternMatchFail` which we can catch and turn into an HTTP "Not Found" (404) response. Split into a hopefully near-final architecture, but still need to use CrudGenerator, read from the IMDB.db, and create the final HTML pages.



**Prerequisite:** in order to use CrudGenerator for Template Haskell, the database must already exist with the expected Schema; therefore, I created a Cabal custom build which also runs InitializeDatabase for the current directory to create IMDB.db in the expected format if it does not already exist.

MovieSearchSite/Setup.hs is identical to PopulateDatabase/Setup.hs from earlier.



The executable itself which starts the server and chooses what router, controller, errorController, and base response (headers and outer HTML that wraps the body).

**Note:** be careful, when the server is running, it appears to capture standard input somehow. It does not respond to Ctrl+C to kill the executable so you will have to kill the process via another mechanism.

MovieSearchSite/app/Main.hs :

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import MovieSearchSite.Controller
    ( controller
    )
import MovieSearchSite.ErrorController
    ( errorController
    )
import MovieSearchSite.Router
    ( router
    , renderRoute
    )
import Network.HTTP.Server
    ( serverWith
    , defaultConfig
    , Config(..)
    , Response(..)
    , StatusCode(..)
    )
import Network.HTTP.Server.Response
    ( reason
    , statusCodeTriplet
    )
import Network.HTTP.Server.Logger
    ( stdLogger
    )
import Network.HTTP.Headers
    ( Header(..)
    , HeaderName(..)
    )
import Control.Monad.Except
    ( runExceptT
    )
import Data.ByteString.Lazy
    ( length
    )
import Data.Time.Clock
    ( getCurrentTime
    )
import Data.Time.Format
    ( formatTime
    , defaultTimeLocale
    )
import Text.Hamlet
    ( hamlet
    )
import Text.Blaze.Html.Renderer.Utf8
    ( renderHtml
    )

main = serverWith
           defaultConfig
           { srvLog     = stdLogger
           , srvPort    = 8888
           }
           handler

    where
    handler _ url request =
        do result <- runExceptT (controller router url request) >>=
                         \case
                             Right (page, body) -> pure (OK, page, body)
                             Left err           -> return $ errorController err
           html result <$> getCurrentTime

        where
        html (code, page, body) now =
            Response
            { rspCode    = statusCodeTriplet code
            , rspReason  = reason code
            , rspHeaders = makeHeaders
            , rspBody    = resp
            }

            where
            resp =
                renderHtml $ [hamlet|
                        $doctype 5
                            <html>
                                <head>
                                    <title>Movie Search Site - #{page}
                                    <link rel="icon" href="data:,">
                                <body>
                                    ^{body}
                    |]
                    renderRoute

            makeHeaders =
                [ Header HdrDate rfc1123Date
                , Header HdrExpires "-1"
                , Header HdrCacheControl "no-store"
                , Header HdrContentType "text/html; charset=utf-8"
                , Header HdrServer "COT6900 DavidBruck MovieSearchSite"
                , Header HdrContentLength $ show $
                      Data.ByteString.Lazy.length resp
                ]

                where
                rfc1123Date =
                    formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT" now
```



First of multiple Library modules, `MovieSearchSite.Controller` checks for a valid HTTP method (it only accepts HTTP GET but not POST) as well as handles invalid paths with 404. Otherwise, calls the router to serve the route-specific content.

src/MovieSearchSite/Controller.hs :

```haskell
module MovieSearchSite.Controller
    ( controller
    ) where

import MovieSearchSite.Router
    ( MovieRoute
    )
import MovieSearchSite.Exceptions
    ( MethodNotAllowedException(..)
    , NotFoundException(..)
    )
import Control.Monad.Except
    ( when
    , runExceptT
    , throwError
    , ExceptT
    )
import Network.HTTP.Server
    ( Request(..)
    , RequestMethod(..)
    )
import Network.URL
    ( URL(..)
    , URLType(..)
    , exportParams
    )
import Control.Exception
    ( toException
    , fromException
    , SomeException
    , PatternMatchFail(..)
    )
import Control.Monad.Catch
    ( catchIf
    )
import Control.Error.Util
    ( syncIO
    )
import Data.ByteString.Lazy
    ( ByteString
    )
import Text.Hamlet
    ( HtmlUrl
    )

controller :: (String -> [(String, String)] -> ExceptT SomeException IO (String, HtmlUrl MovieRoute)) -> URL -> Request ByteString ->
    ExceptT SomeException IO (String, HtmlUrl MovieRoute)
controller router url request =
    do let method   = rqMethod request
       let path     = url_path url
       when (method /= GET) $ throwError $ toException
           $ MethodNotAllowedException method
       catchIf
           isNotFound
           (router path params)
           catchNotFound

    where
    isNotFound ex
        | Just (PatternMatchFail _) <- fromException ex = True
        | otherwise                                     = False

    catchNotFound :: SomeException ->
                         ExceptT SomeException IO (String, HtmlUrl MovieRoute)
    catchNotFound _     = throwError $ toException $ NotFoundException
                              $ exportPath params
    method              = rqMethod request
    path                = url_path url
    params              = url_params url
    exportPath []       = path
    exportPath params   = path ++ ('?' : exportParams params)
```



A fallback "error" controller, src/MovieSearchSite/ErrorController.hs :

```haskell
{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.ErrorController
    ( errorController
    ) where

import MovieSearchSite.Router
    ( MovieRoute
    )
import MovieSearchSite.Exceptions
    ( MethodNotAllowedException(..)
    , NotFoundException(..)
    )
import Network.HTTP.Server
    ( StatusCode(..)
    )
import Control.Exception
    ( fromException
    , SomeException
    )
import Text.Hamlet
    ( hamlet
    , HtmlUrl
    )

errorController :: SomeException -> (StatusCode, String, HtmlUrl MovieRoute)
errorController err
    | Just (MethodNotAllowedException method) <- fromException err =
          ( MethodNotAllowed
          , "Method Not Allowed"
          , [hamlet|
                <h1>Method Not Allowed<br/>
                    <small>Method: #{show method}
            |]
          )
    | Just (NotFoundException path) <- fromException err =
          ( NotFound
          , "Not Found"
          , [hamlet|
                <h1>Not Found<br/>
                    <small>Path: #{path}
            |]
          )
    | otherwise =
          ( InternalServerError
          , "Unknown Error"
          , [hamlet|
                <h1>Unknown Error
                    <small>#{show err}
            |]
          )
```



Custom `Exception` instances which we can Monadic `throwError` to serve appropriate error responses. Can contain data like the "Not Found" path or what HTTP method was not allowed (e.g. HTTP POST), src/MovieSearchSite/Exceptions.hs :

```haskell
module MovieSearchSite.Exceptions
    ( MethodNotAllowedException(..)
    , NotFoundException(..)
    ) where

import Control.Exception
    ( Exception
    , PatternMatchFail
    )
import Network.HTTP.Server
    ( RequestMethod
    )

newtype MethodNotAllowedException = MethodNotAllowedException
    { notAllowedMethod :: RequestMethod
    } deriving (Show)
newtype NotFoundException = NotFoundException
    { notFoundPath :: String
    } deriving (Show)

instance Exception MethodNotAllowedException
instance Exception NotFoundException
```



Router which maps defined routes to their controllers and also renders URLs which will navigate to those routes, src/MovieSearchSite/Router.hs :

```haskell
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
```



Unfinished "Home" controller for the root of the website, src/MovieSearchSite/Home.hs :

```haskell
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
```



Unfinished "Search" controller for searching the website, src/MovieSearchSite/Search.hs :

```haskell
{-# LANGUAGE QuasiQuotes #-}
module MovieSearchSite.Search
    ( searchController
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

searchController :: String -> ExceptT SomeException IO (String, HtmlUrl a)
searchController query =
    return
        ( "Search"
        , [hamlet|
                <h1>Search #{query}
          |]
        )
```



Unfinished "Title" controller for showing details for a single title, src/MovieSearchSite/Title.hs :

```haskell
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
                <h1>Title
          |]
        )
```

