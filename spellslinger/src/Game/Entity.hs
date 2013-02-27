{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Entity (
        Entity(..)
    ) where

import Game.Engine ( Game, Picture )

class Entity w a where
    draw :: w -> a -> Picture
    tickVisual :: a -> a
    tick :: a -> Game w ()
