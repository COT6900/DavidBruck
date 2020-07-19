module MovieSearchSite.Controller
    ( controller
    , Router
    ) where

import MovieSearchSite.MovieRoute
    ( MovieRoute
    )
import MovieSearchSite.Exceptions
    ( MethodNotAllowedException(..)
    , NotFoundException(..)
    )
import MovieSearchSite.Response
    ( HtmlResponse
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
import Data.ByteString.Lazy
    ( ByteString
    )
    
type Router =
    String -> [(String, String)] ->
    ExceptT SomeException IO HtmlResponse

controller ::
    Router -> URL -> Request ByteString ->
    ExceptT SomeException IO HtmlResponse
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
                         ExceptT SomeException IO HtmlResponse
    catchNotFound _     = throwError $ toException $ NotFoundException
                              $ exportPath params
    method              = rqMethod request
    path                = url_path url
    params              = url_params url
    exportPath []       = path
    exportPath params   = path ++ ('?' : exportParams params)
