module Types (
        Direction(..), EntityId(..)
    ) where

data Direction = North | West | South | East
               deriving ( Eq, Show )

newtype EntityId = EntityId Int
                 deriving ( Eq, Ord, Show )
