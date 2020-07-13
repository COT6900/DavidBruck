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
