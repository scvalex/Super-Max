{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Entities.Zombie (
        Zombie, EntityParameters(..), State(..),
        setPosition, getPosition, getState, alertZombie, roamTowards
    ) where

import Common ( intRectangle, fromAreaCoordinates )
import Control.Applicative ( (<$>) )
import Data.Monoid ( Monoid(..) )
import qualified Data.Set as S
import SuperMax ( Picture(..), Colour(..), mkUid, randomR
                , Entity(..), EntityId(..), Position(..) )
import Types ( Direction(..), randomDirection )

data State = Roaming Direction
           | Following
             deriving ( Eq, Show )

-- FIXME Zombies should have emoticons drawn on their faces.

data Zombie = Zombie
    { getZombieId         :: EntityId
    , getZombieState      :: State
    , getZombiePosition   :: Position
    , getZombieAreaBounds :: (Int, Int, Int, Int)
    , getZombieVisualTint :: Double
    , getZombieAlerted    :: Maybe Int
    } deriving ( Eq, Show )

instance Entity Zombie where
    data EntityParameters Zombie = RandomZombie
        { getAreaBounds :: (Int, Int, Int, Int)
        }

    init zp@(RandomZombie { getAreaBounds = (x1, y1, x2, y2) }) = do
        zid <- EntityId <$> mkUid
        xz <- randomR (x1, x2)
        yz <- randomR (y1, y2)
        dir <- randomDirection
        return (Zombie { getZombieId         = zid
                       , getZombieState      = Roaming dir
                       , getZombiePosition   = Position (xz, yz)
                       , getZombieAreaBounds = getAreaBounds zp
                       , getZombieVisualTint = pi
                       , getZombieAlerted    = Nothing
                       })

    eid (Zombie { getZombieId = zid }) = zid

    occupiedPositions (Zombie { getZombiePosition = pos }) =
        S.singleton pos

    draw (Zombie { getZombiePosition   = Position (xz, yz)
                 , getZombieAreaBounds = bounds
                 , getZombieVisualTint = tint
                 , getZombieAlerted    = malerted }) =
        let col = Colour (RGBA 0 (180 + floor (75.0 * sin tint)) 0 255) in
        fromAreaCoordinates bounds $
        mconcat [ case malerted of
                       Nothing ->
                           mempty
                       Just n ->
                           let s = fromIntegral n * 0.1 in
                           mconcat [ col $
                                     FilledRectangle (fromIntegral xz - (s + 0.2))
                                                     (fromIntegral yz - (s + 0.2))
                                                     (1.0 + 2.0 * s + 0.4)
                                                     (1.0 + 2.0 * s + 0.4)
                                   , Colour (RGBA 0 0 0 255) $
                                     FilledRectangle (fromIntegral xz - s)
                                                     (fromIntegral yz - s)
                                                     (1.0 + 2.0 * s)
                                                     (1.0 + 2.0 * s)
                                   ]
                , col $
                  intRectangle xz yz 1 1
                ]

    tickVisual z@(Zombie { getZombieVisualTint = tint
                         , getZombieAlerted    = malerted }) =
        z { getZombieVisualTint = tint + pi / 16
          , getZombieAlerted = case malerted of
                                    Nothing -> Nothing
                                    Just 0  -> Nothing
                                    Just n  -> Just (n - 1)
          }

setPosition :: Zombie -> Position -> Zombie
setPosition z pos = z { getZombiePosition = pos }

getPosition :: Zombie -> Position
getPosition (Zombie { getZombiePosition = pos }) = pos

getState :: Zombie -> State
getState (Zombie { getZombieState = state }) = state

alertZombie :: Zombie -> Zombie
alertZombie z@(Zombie { getZombieState = Following }) =
    z
alertZombie z =
    z { getZombieState   = Following
      , getZombieAlerted = Just 6 }

roamTowards :: Zombie -> Direction -> Zombie
roamTowards z dir = z { getZombieState = Roaming dir }
