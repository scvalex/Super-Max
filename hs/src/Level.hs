module Level
    ( Level (..)
    , Background
    , tileEdge
    , Tile (..)
    , TileCol (..)
    , levelHeight
    , drawLevel

      -- * Tests
    , dummyLevel
    ) where

import Graphics.Gloss

data Level = Level
     { levelBgr  :: Background
     , levelCols :: [TileCol]
     } deriving (Show, Eq)

-- | A 'Background' is a 'Color' and a 'Picture' that will be repeated on the
--   x-axis.
data Background = Background Color Picture
    deriving (Show, Eq)

tileEdge :: Int
tileEdge = 30

-- | A tile is a 'tileEdge'x'tileEdge' 'Picture'. They're the building
--   blocks for levels.
--
--   The image must use space coordinates [-15,15], on both axis. In other
--   words, the tile is centerd on the axis.
newtype Tile = Tile {unTile :: Picture}
    deriving (Show, Eq)

-- | A column of tiles. A 'Level' is a list of those.
data TileCol = TileCol
     { colUp :: [Maybe Tile]     -- ^ The tiles from row 0 up
     , colDown :: [Maybe Tile]   -- ^ The tiles from row -1 down
     } deriving (Show, Eq)

levelHeight :: Level -> Int
levelHeight = maximum . map colHeight . levelCols

levelWidth :: Level -> Int
levelWidth (Level {levelCols = cols}) = tileEdge * length cols

colHeight :: TileCol -> Int
colHeight (TileCol {colUp = up, colDown = down}) = tileEdge * (length up + length down)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

drawLevel :: Level -> Picture
drawLevel (Level {levelCols = cols, levelBgr = (Background _ bgrp)}) =
    pictures [tiles, bgr]
  where
    tiles = pictures . map (\(i, c) -> translate (fi i) 0 c) .
            zip [0,tileEdge..] . map drawCol $ cols
    bgr   = blank

drawCol :: TileCol -> Picture
drawCol (TileCol {colUp = up, colDown = down}) =
    pictures . map (\(i, t) -> translate 0 (fi i) t) $ tiles
  where
    unTileM Nothing  = blank
    unTileM (Just t) = unTile t

    tiles = zip [0,tileEdge..] (map unTileM up) ++
            zip [-tileEdge,-tileEdge*2..] (map unTileM down)

dummyLevel :: Picture -> Level
dummyLevel pic =
    Level { levelBgr  = Background black blank
          , levelCols = replicate 10 (TileCol [Just tile] []) ++
                        replicate 10 (TileCol [Just (Tile pic)] [])
          }
  where
    tile  = Tile $ color red $
            polygon [(-15,15), (15,15), (15, -15), (-15, -15)]
