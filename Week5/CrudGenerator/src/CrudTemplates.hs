module CrudTemplates
    ( generateCrud
    ) where

import DatabaseHelpers
    ( withDatabase
    , getSchema
    , SchemaColumn(..)
    , SchemaTable(..)
    )
import Data.Text
    ( unpack
    )
import Data.Char
    ( toUpper
    , toLower
    )
import Data.Maybe
    ( isNothing
    )
import Control.Applicative
    ( (<$>)
    )

import Control.Monad.Except
    ( runExceptT
    )
import Control.Exception
    ( toException
    , throw
    , SomeException
    , Exception
    )

import Control.Monad
import Language.Haskell.TH

newtype UnhandledSchemaException = UnhandledSchemaException
    { error :: String
    } deriving (Show)

instance Exception UnhandledSchemaException

generateCrud :: String -> Q [Dec]
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
        do let dataTypes = generateDataTypes tables
           -- queries      <- generateQueries tables
           return dataTypes

    generateDataTypes []        = []
    generateDataTypes (x:xs)    =
        DataD
            []
            capitalizedName
            []
            Nothing
            [fields]
            []
        : generateDataTypes xs

        where
        name = unpack $ tableName x
        capitalizedName = mkName $ capitalize name
        columns = crudFields name $ tableColumns x
        fields = RecC capitalizedName columns

    -- generateQueries []      = []
    -- generateQueries (x:xs)  =
    --     do 

    capitalize []               = []
    capitalize (x:xs)           = toUpper x : xs

    lowercase []                = []
    lowercase (x:xs)            = toLower x : xs

    crudFields tableName []     = []
    crudFields tableName (x:xs) =
        ( mkName (lowercase tableName ++ "_" ++ unpack (name x))
        , Bang NoSourceUnpackedness NoSourceStrictness
        , columnType (unpack <$> type' x) $ notnull x
        )
        : crudFields tableName xs

    columnType sqlType
        | isNothing sqlType         = makeType "String"
        | sqlType == Just "TEXT"    = makeType "String"
        | sqlType == Just ""        = makeType "String"
        | sqlType == Just "INTEGER" = makeType "Int"
        | otherwise                 = throw $ toException
                                          $ UnhandledSchemaException
                                          $ "unhandled SQL column type "
                                                ++ show sqlType
    makeType haskellType notnull =
        if notnull == 1
        then ConT $ mkName haskellType
        else AppT (makeType "Maybe" 1) (makeType haskellType 1)
