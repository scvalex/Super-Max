{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module SuperMax.Entity (
        Entity(..), EntityId(..), Position(..), Behaviour(..)
    ) where

import Data.Set ( Set )
import SuperMax.GL ( Game )
import SuperMax.GL.Drawing ( Drawable )

newtype EntityId = EntityId Int
                 deriving ( Eq, Ord, Show )

newtype Position = Position (Int, Int)
                 deriving ( Eq, Ord, Show )

class (Drawable a) => Entity a where
    data EntityParameters a :: *

    init :: EntityParameters a -> Game w a
    eid :: a -> EntityId
    occupiedPositions :: a -> Set Position
    tickVisual :: a -> a

class (Entity a) => Behaviour w a where
    behave :: a -> Game w a
