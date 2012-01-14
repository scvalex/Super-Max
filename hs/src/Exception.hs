{-# LANGUAGE DeriveDataTypeable #-}
module Exception
    ( SDLException(..)
    ) where

import Data.Data (Typeable)
import Control.Exception

data SDLException
    = FailedBlitting
    | FailedSetVideoMode
    deriving (Eq, Show, Typeable)

instance Exception SDLException