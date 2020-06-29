module DatabaseHelpers
    ( getSchema
    , withDatabase
    , databaseQuery
    , SchemaColumn(..)
    , SchemaTable(..)
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
            , True              <-
                isFullTextTable
                    $ take (length tableName - endingLength) tableName =
                        FullTextStorage
            | True              <- isFullTextTable tableName    = FullText
            | otherwise                                         = Table
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
        throwIO $ SQLStringException "expecting just SQLText column"

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
        validMaybeText maybeText
            | SQLNull   <- maybeText    = True
            | SQLText _ <- maybeText    = True
            | otherwise                 = False
        maybeText SQLNull               = Nothing
        maybeText (SQLText value)       = Just value
    schemaTransformer _ =
        throwIO $ SQLStringException
            "expecting schema columns (Int, Text, Text, Int, Text NULL, Int)"

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
