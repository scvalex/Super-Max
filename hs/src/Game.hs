{-# LANGUAGE ExistentialQuantification #-}
module Game
    ( Time
    , Step(..)
    , HasSurface(..)
    , Point
    , BBox
    , colliding
    , Rect(..)
    , translate
    , Object(..)

    , Game(..)
    , stepGame

    , LevelObject(..)
    , Level(..)
    , buildLevel

    , Ani(..)
    , applyAni
    , aniSur
    , staticAni
    , Mov(..)
    , applyMov
    , movPos
    , staticMov
    , dynamicMov

    , Direction(..)
    , Player(..)
    ) where

import Control.Monad
import Data.Word (Word32)

import Graphics.UI.SDL (Surface, Rect(..))
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym

import Vector
import Common

import Debug.Trace (trace)

-------------------------------------------------------------------------------

type Time = Word32

class Step a where
    step :: Time -> Event -> Game -> a -> IO a

class HasSurface a where
    surface :: a -> Surface

-- | A point in space
type Point = (Int, Int)

-- | A bounding box
type BBox = [Rect]

translate :: Int -> Int -> Rect -> Rect
translate x' y' (Rect x y w h) = Rect (x' + x) (y' + y) w h

class (HasSurface o, Step o) => Object o where
    -- | The 'Rect' in which the object resides
    rect   :: o -> Rect
    -- | The bounding box of the object.
    bbox   :: o -> BBox

data Collision
    = North | South | East | West
    deriving (Eq, Show)

colliding :: BBox -> BBox -> Maybe Collision
colliding bb1 bb2 = msum [f b1 b2 | b1 <- bb1, b2 <- bb2]
  where
    f (Rect l1 t1 w1 h1) (Rect l2 t2 w2 h2) =
        let r1 = l1 + w1
            b1 = t1 + h1
            r2 = l2 + w2
            b2 = t2 + h2
            r  = b1 <= t2 || t1 >= b2 || r1 <= l2 || l1 >= r2
        in  if r then Nothing else Just South

-------------------------------------------------------------------------------

data Game = Game
    { gamePlayer :: Player
    , gameLevel  :: Level
    }

stepGame :: Time -> Event -> Game -> IO Game
stepGame delta ev g@(Game {gamePlayer = p, gameLevel = lvl}) = do
    lvl' <- step delta ev g lvl
    p'   <- step delta ev g p
    return $ g {gamePlayer = p', gameLevel = lvl'}

-------------------------------------------------------------------------------

data LevelObject = forall o. Object o => LevelObject o

data Level = Level
    { levelObjects :: [LevelObject]
    , levelGravity :: Vector
    }

instance Step Level where
    step d ev w (Level {levelObjects = os, levelGravity = g}) = do
        os' <- forM os $ \(LevelObject o) -> fmap LevelObject $ step d ev w o
        return $ Level {levelObjects = os', levelGravity = g}

buildLevel :: Double -> [LevelObject] -> Level
buildLevel g os = Level { levelObjects = os
                        , levelGravity = (0, g)
                        }

-------------------------------------------------------------------------------

-- | An animation.
data Ani = Ani Surface (Time -> Ani)

applyAni :: Ani -> Time -> Ani
applyAni (Ani _ f) fl = f fl

aniSur :: Ani -> Surface
aniSur (Ani sur _) = sur

staticAni :: Surface -> Ani
staticAni s = Ani s (\_ -> staticAni s)

-- | A movement.
data Mov = Mov Point (Time -> Mov)

applyMov :: Mov -> Time -> Mov
applyMov (Mov _ f) delta = f delta

movPos :: Mov -> Point
movPos (Mov p _) = p

staticMov :: Point -> Mov
staticMov p = Mov p (\_ -> staticMov p)

dynamicMov :: (a -> Time -> Point -> (a, Point)) -> (a, Point) -> Mov
dynamicMov f (x, p) = Mov p $ \delta -> dynamicMov f (f x delta p)

-------------------------------------------------------------------------------

data Direction = DirLeft | DirRight
    deriving (Eq, Show)

data Player = Player
    { playerAni  :: Ani
    , playerPos  :: Vector
    , playerDim  :: Vector
    , playerBBox :: BBox
    , playerAcc  :: Double          -- ^ The walking acceleration
    , playerDir  :: Maybe Direction -- ^ The key that the user is pressing
    , playerX    :: Double          -- ^ The X velocity
    , playerY    :: Double          -- ^ The Y velocity
    , playerMaxX :: Double          -- ^ The maximum velocity on the X axis
    }

-- | Moves the player by a certain 'Vector'.
movePlayerVec :: Vector -> Level -> Player
              -> Either (Player, Collision) Player
              -- ^ Right if the movement has succeeded, Left if the player has
              --   hit a collision at some point, including the collision and
              --   the new player.
movePlayerVec origVec lvl origP =
    case move origVec origP of
        Right p -> Right p
        Left _  -> Left $ attach origP
  where
    move vec p =
        let p'    = p {playerPos = vec `addV` playerPos p}
            collM = msum $ map (\(LevelObject o) -> colliding (bbox p') (bbox o))
                           (levelObjects lvl)
        in  case collM of
                Nothing   -> Right p'
                Just coll -> Left coll

    attach p =
        case move (normaliseV origVec) p of
            Right p'  -> attach p'
            Left coll -> (p, coll)

movePlayerG :: Time -> Vector -> Level -> Player
            -> Either (Player, Collision) Player
movePlayerG t vec lvl@(Level {levelGravity = g}) p
    = movePlayerVec (vec `addV` (fi t `mulSV` g)) lvl p

movePlayer :: Time -> Level -> Player -> Player
movePlayer t lvl p =
    case movePlayerG t (0, 0) lvl p of
        Left (p', _) -> trace "colliding" p'
        Right p'     -> trace "going" p'

-- movePlayerG :: Time -> Level -> Vector -> Player -> Player
-- movePlayerG t lvl oldVec p =
--     case movePlayerPt vec lvl p of
--         Nothing -> attach (normaliseV vec) p
--         Just p' -> p'
--   where
--     g    = fi t `mulSV` levelGravity lvl
--     vec  = g `addV` oldVec

--     attach vec' p' =
--         case movePlayerPt vec' lvl p' of
--             Nothing  -> p'
--             Just p'' -> attach vec' p''

-- movePlayer :: Time -> Level -> Player -> Player
-- movePlayer t lvl p@(Player {playerDir = Nothing}) = movePlayerG t lvl (0,0) p
-- movePlayer t lvl p@(Player {playerDir = Just dir, playerWalk = walk}) =
--     movePlayerG t lvl (fi t `mulSV` (walk `mulSV` vec)) p
--   where
--     vec = case dir of
--               Left  -> (-1, 0)
--               Right -> (1, 0)

instance Step Player where
    step t (KeyDown k) (Game {gameLevel = lvl}) p = return $
        case symKey k of
            SDLK_LEFT  -> movePlayer t lvl $ p {playerDir = Just DirLeft}
            SDLK_RIGHT -> movePlayer t lvl $ p {playerDir = Just DirRight}
            _          -> p
    step t (KeyUp k) (Game {gameLevel = lvl}) p@(Player {playerDir = Just dir})
        | symKey k == SDLK_LEFT && dir == DirLeft ||
          symKey k == SDLK_RIGHT && dir == DirRight =
          return $ p {playerDir = Nothing}
        | otherwise = return $ movePlayer t lvl p
    step t _ (Game {gameLevel = lvl}) p = return $ movePlayer t lvl p

instance HasSurface Player where
    surface (Player {playerAni = ani}) = aniSur ani

instance Object Player where
    rect (Player {playerPos = (x, y), playerDim = (w, h)}) =
        Rect (round x) (round y) (round w) (round h)
    bbox (Player {playerPos = (x, y), playerBBox = box})   =
        map (translate (round x) (round y)) box
