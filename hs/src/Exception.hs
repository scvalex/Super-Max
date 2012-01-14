{-# LANGUAGE DeriveDataTypeable #-}
module Exception where

import Data.Data (Typeable)
import Control.Exception

data SDLException
    = FailedBlitting
    | FailedSetVideoMode
    deriving (Eq, Show, Typeable)

instance Exception SDLException