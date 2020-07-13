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
