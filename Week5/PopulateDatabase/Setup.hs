import Distribution.Simple
    ( defaultMainWithHooks
    , simpleUserHooks
    , UserHooks(..)
    )
import DatabaseHelpers
    ( withDatabase
    , databaseQuery
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
import Control.Monad.Except
    ( runExceptT
    )
import Control.Error.Util
    ( syncIO
    )
import System.IO
    ( stdout
    , stderr
    , hFlush
    , hClose
    , hSeek
    , hGetContents
    , hPutStrLn
    , SeekMode(..)
    )
import System.Exit
    ( ExitCode(..)
    )

main = defaultMainWithHooks buildHooked

    where
    buildHooked = simpleUserHooks {buildHook = initializeDatabaseFirst}

    initializeDatabaseFirst pdesc lbinfo uhooks bflags =
        do result <- runExceptT $ initializeDatabase `withDatabase` "IMDB.db"
           case result of
               Left err -> error $ "Error: " ++ show err
               Right _  -> buildHook simpleUserHooks pdesc lbinfo uhooks bflags

        where
        initializeDatabase conn =
            do existing <- databaseQuery
                               ( "SELECT 1\n" ++
                                 "FROM sqlite_master"
                               )
                               conn
                               []
                               return
               syncIO $ when (null existing) runInitialize

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

        getContents h purpose =
            case h of
                Just h  -> hGetContents h
                _       -> return $ "Error: " ++ purpose ++ " handle Nothing"
