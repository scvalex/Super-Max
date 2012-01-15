{-# LANGUAGE ExistentialQuantification, Rank2Types, TupleSections #-}
module Game
    ( Time
    , Step(..)
    , HasSurface(..)
    , Point
    , BBox
    , distinct
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
import Prelude hiding (Either (..))

import Graphics.UI.SDL (Surface, Rect(..))
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym

import Common

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

distinct :: (Object o1, Object o2) => o1 -> o2 -> Bool
distinct o1 o2 = or [f b1 b2 | b1 <- bbox o1, b2 <- bbox o2]
  where
    f (Rect l1 t1 w1 h1) (Rect l2 t2 w2 h2) =
        let r1 = l1 + w1
            b1 = t1 + h1
            r2 = l2 + w2
            b2 = t2 + h2
        in  b1 <= t2 || t1 >= b2 || r1 <= l2 || l1 >= r2

-------------------------------------------------------------------------------

data Game = Game
    { gamePlayer :: Player
    , gameActDir :: [Direction]
    , gameLevel  :: Level
    }

stepGame :: Time -> Event -> Game -> IO Game
stepGame delta ev g@(Game {gamePlayer = p, gameLevel = lvl}) = do
    lvl' <- step delta ev g lvl
    p'   <- step delta ev g p
    return $ g {gamePlayer = p', gameLevel = lvl'}

-------------------------------------------------------------------------------

data LevelObject = forall o. Object o => LevelObject o

newtype Level = Level {levelObjects :: [LevelObject]}

instance Step Level where
    step d ev w (Level {levelObjects = os}) = do
        os' <- forM os $ \(LevelObject o) -> fmap LevelObject $ step d ev w o
        return $ buildLevel os'

buildLevel :: [LevelObject] -> Level
buildLevel = Level

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

data Direction = Left | Right
    deriving (Eq, Show)

data Player = Player
    { playerAni  :: Ani
    , playerPos  :: Point
    , playerDim  :: (Int, Int)
    , playerBBox :: BBox
    , playerDir  :: Maybe Direction
    }

velocity :: Float
velocity = 0.3

movePlayerPt :: Int -> Level -> Player -> Maybe Player
movePlayerPt d lvl p@(Player {playerPos = (x, y)})
    | coll      = Just p'
    | otherwise = Nothing
  where
    p'   = p {playerPos = (x + d, y)}
    coll = and $ map (\(LevelObject o) -> distinct p' o) (levelObjects lvl)

movePlayer :: Time -> Level -> Player -> Player
movePlayer _ _ p@(Player {playerDir = Nothing})    = p
movePlayer d lvl p@(Player {playerDir = Just dir}) =
    case dir of
        Left  -> move (-1)
        Right -> move 1
  where
    move sign =
        case movePlayerPt (sign * round (fi d * velocity)) lvl p of
            Nothing -> attach sign p
            Just p' -> p'
    attach sign p' =
        case movePlayerPt sign lvl p' of
            Nothing  -> p'
            Just p'' -> attach sign p''

instance Step Player where
    step delta (KeyDown k) (Game {gameLevel = lvl}) p = return $
        case symKey k of
            SDLK_LEFT  -> movePlayer delta lvl $ p {playerDir = Just Left}
            SDLK_RIGHT -> movePlayer delta lvl $ p {playerDir = Just Right}
            _          -> p
    step delta (KeyUp k) (Game {gameLevel = lvl}) p@(Player {playerDir = Just dir})
        | symKey k == SDLK_LEFT && dir == Left || symKey k == SDLK_RIGHT && dir == Right =
          return $ p {playerDir = Nothing}
        | otherwise = return $ movePlayer delta lvl p
    step delta _ (Game {gameLevel = lvl}) p = return $ movePlayer delta lvl p

instance HasSurface Player where
    surface (Player {playerAni = ani}) = aniSur ani

instance Object Player where
    rect (Player {playerPos = (x, y), playerDim = (w, h)}) = Rect x y w h
    bbox (Player {playerPos = (x, y), playerBBox = box})   =
        map (translate x y) box
