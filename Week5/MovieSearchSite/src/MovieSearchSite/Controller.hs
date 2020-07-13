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
