{-# LANGUAGE ExistentialQuantification, Rank2Types, TupleSections #-}
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
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
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

colliding :: BBox -> BBox -> Bool
colliding bs1 bs2 = or [f b1 b2 | b1 <- bs1, b2 <- bs2]
  where
    f (Rect l1 t1 w1 h1) (Rect l2 t2 w2 h2) =
        let r1 = l1 + w1
            b1 = t1 + h1
            r2 = l2 + w2
            b2 = t2 + h2
        in  b1 <= t2 || t1 >= b2 || r1 <= l2 || l1 >= r2

class (HasSurface o, Step o) => Object o where
    -- | The 'Rect' in which the object resides
    rect   :: o -> Rect
    -- | The bounding box of the object.
    bbox   :: o -> BBox

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

type Tag = Int

type Grid = Map (Int, Int) IntSet

mkGrid :: [((Int, Int), Tag)]    -- ^ Initial tags
       -> Grid
mkGrid = go M.empty
  where
    go g []              = g
    go g ((k, t) : rest) =
        case M.lookup k g of
            Nothing -> go (M.insert k (IS.singleton t) g) rest
            Just s  -> go (M.insert k (IS.insert t s) g) rest

-- lookupGrid :: (Int, Int) -> Grid -> IntSet
-- lookupGrid k g =
--     case M.lookup k g of
--         Nothing -> IS.empty
--         Just s  -> s

gridEdge :: Int
gridEdge = 30

-------------------------------------------------------------------------------

data LevelObject = forall o. Object o => LevelObject o

data Level = Level
    { levelObjects :: IntMap LevelObject
    , levelGrid    :: Grid
    }

instance Step Level where
    step d ev w (Level {levelObjects = los}) = do
        let os = map snd $ IM.toList los
        os' <- forM os $ \(LevelObject o) -> fmap LevelObject $ step d ev w o
        return $ buildLevel os'

buildLevel :: [LevelObject] -> Level
buildLevel os = Level { levelObjects = im
                      , levelGrid    = grid
                      }
  where
    ost  = zip [1..] os
    im   = IM.fromList ost
    grid = mkGrid $ concat [map (,t) (cells $ rect o) | (t, LevelObject o) <- ost]

cells :: Rect -> [(Int, Int)]
cells (Rect x y w h) =
    let x'    = x `div` gridEdge
        y'    = y `div` gridEdge
        spanx = (w + x `mod` gridEdge) `div` gridEdge
        spany = (h + y `mod` gridEdge) `div` gridEdge
    in  [(x'+cx, y'+cy) | cx <- [0..spanx], cy <- [0..spany]]

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
    bbox (Player {playerPos = (x, y), playerBBox = box})   =
        map (translate x y) box
