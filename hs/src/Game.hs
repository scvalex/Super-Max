{-# LANGUAGE ExistentialQuantification #-}
module Game
    ( Time
    , Step(..)
    , HasSurface(..)
    , Point
    , Path
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

import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL (Surface, Rect(..))
import Data.Word (Word32)
import Control.Monad
import Prelude hiding (Either (..))

import Common

-------------------------------------------------------------------------------

type Time = Word32

class Step a where
    step :: Time -> Event -> Game -> a -> IO a

class HasSurface a where
    surface :: a -> Surface

-- | A point in space
type Point = (Int, Int)

-- | A polygon path
type Path = [Point]

translate :: Point -> Point -> Point
translate (x, y) (x', y') = (x' + x, y' + y)

class (HasSurface o, Step o) => Object o where
    -- | The 'Rect' in which the object resides
    rect   :: o -> Rect
    -- | The bounding box of the object.
    bbox   :: o -> Path

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
    step d ev w l = do
        os <- forM (levelObjects l) $
              \(LevelObject o) -> fmap LevelObject (step d ev w o)
        return $ l {levelObjects = os}

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
    , playerBBox :: Path
    , playerDir  :: Maybe Direction
    }

velocity :: Float
velocity = 0.3

movePlayer :: Time -> Player -> Player
movePlayer _ p@(Player {playerDir = Nothing}) = p
movePlayer d p@(Player {playerDir = Just dir, playerPos = (x, y)}) =
    case dir of
        Left  -> p {playerPos = (round (fi x - fi d * velocity), y)}
        Right -> p {playerPos = (round (fi x + fi d * velocity), y)}

instance Step Player where
    step delta (KeyDown k) _ p = return $
        case symKey k of
            SDLK_LEFT  -> movePlayer delta $ p {playerDir = Just Left}
            SDLK_RIGHT -> movePlayer delta $ p {playerDir = Just Right}
            _          -> p
    step delta (KeyUp k) _ p@(Player {playerDir = Just dir})
        | symKey k == SDLK_LEFT && dir == Left || symKey k == SDLK_RIGHT && dir == Right =
          return $ p {playerDir = Nothing}
        | otherwise = return $ movePlayer delta p
    step delta _ _ p = return $ movePlayer delta p

instance HasSurface Player where
    surface (Player {playerAni = ani}) = aniSur ani

instance Object Player where
    rect (Player {playerPos = (x, y), playerDim = (w, h)}) = Rect x y w h
    bbox (Player {playerPos = pos, playerBBox = box}) =
        map (translate pos) box
