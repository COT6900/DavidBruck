{-# LANGUAGE DeriveGeneric #-}
module Main where

import DatabaseHelpers
    ( withDatabase
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
import Data.Text
    ( Text
    )
import Data.Text.Lazy.Encoding
    ( decodeUtf8
    )
import Data.Csv
    ( decodeWith
    , defaultDecodeOptions
    , DecodeOptions(..)
    , Header
    , FromRecord
    , HasHeader(..)
    )
import Data.Either
    ( either
    )
import Data.Char
    ( ord
    )
import Data.Vector
    ( head
    , Vector
    )
import Data.ByteString.Lazy
    ( fromStrict
    )
import Data.Function
    ( (&)
    )
import GHC.Generics
    ( Generic
    )

newtype DecodeException = DecodeException
    { decodeError :: String
    } deriving (Show)

instance Exception DecodeException

data Title = Title
    { tconst            :: !Text
    , titleType         :: !Text
    , primaryTitle      :: !Text
    , originalTitle     :: !Text
    , isAdult           :: !Text
    , startYear         :: !Text
    , endYear           :: !Text
    , runtimeMinutes    :: !Text
    , genres            :: !Text
    }
    deriving (Generic, Show)

instance FromRecord Title

main = do result <- runExceptT $ initializeDatabase `withDatabase` "IMDB.db"
          case result of
              Left err -> Prelude.error $ "Error: " ++ show err
              Right _  -> putStrLn "Success"

    where
    initializeDatabase conn =
        do let titleBasicsURL = "https://datasets.imdbws.com/title.basics.tsv.gz"
           syncIO $ putStrLn $
               "Downloading " ++ titleBasicsURL ++ " ..."
           downloaded       <- syncIO (parseRequest titleBasicsURL)
                                   >>= syncIO . httpBS
           syncIO $ putStrLn "Unzipping and decoding ..."
           let decoded = getResponseBody downloaded
                             & fromStrict
                             & decompress
           
           vector <- either
                         (throwError . toException . DecodeException)
                         return
                         (decodeWith
                             defaultDecodeOptions
                             { decDelimiter = fromIntegral (ord '\t')
                             }
                             NoHeader
                             decoded
                             :: Either String (Vector Title))

           syncIO $ putStrLn "Adding records to database ..."
           syncIO $ print $ Data.Vector.head vector
