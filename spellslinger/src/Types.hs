module Types (
        Direction(..), EntityId(..), Position(..)
    ) where

data Direction = North | West | South | East
               deriving ( Eq, Show )

newtype EntityId = EntityId Int
                 deriving ( Eq, Ord, Show )

newtype Position = Position (Int, Int)
                 deriving ( Eq, Show )