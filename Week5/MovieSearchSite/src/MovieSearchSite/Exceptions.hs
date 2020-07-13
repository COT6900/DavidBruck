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
