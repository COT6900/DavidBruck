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
           return $ dataTypes ++ transformers ++ queries

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
        unpackedTableName = unpack $ tableName x
        capitalizedName = mkName $ capitalize unpackedTableName
        lowecasedName = lowercase unpackedTableName

        crudFields []     = pure []
        crudFields (x:xs) =
            do field <- varBangType
                            (mkName (lowecasedName ++ "_" ++ unpack (name x)))
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
        case tableType x of
            Table       -> do tableQueries  <- generateTableQueries columns
                              rest          <- generateQueries xs
                              return $ tableQueries ++ rest
            FullText    -> do fullText      <- generateFullTextQueries columns
                              rest          <- generateQueries xs
                              return $ fullText ++ rest
            _           -> throw $ toException
                               $ UnhandledSchemaException
                               $ "unhandled SQL table type " ++ show x

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
        columnType = type' column
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
