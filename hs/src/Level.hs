module Level
    ( Level (..)
    , Background
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

-- | A 'Background' is a 'Picture' that will be repeated on the x-axis.
type Background = Picture

tileEdge :: Int
tileEdge = 30

-- | A tile is a 10x10 'Picture'. They're the building blocks for levels.
newtype Tile = Tile {unTile :: Picture}
    deriving (Show, Eq)

-- | A column of tiles. A 'Level' is a list of those.
data TileCol = TileCol
     { colUp :: [Maybe Tile]     -- ^ The tiles from row 0 up
     , colDown :: [Maybe Tile]   -- ^ The tiles from row -1 down
     } deriving (Show, Eq)

levelHeight :: Level -> Int
levelHeight = maximum . map colHeight . levelCols

colHeight :: TileCol -> Int
colHeight (TileCol {colUp = up, colDown = down}) = tileEdge * (length up + length down)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

drawLevel :: Level -> Picture
drawLevel (Level {levelCols = cols}) =
    pictures . map (\(i, c) -> translate (fi i) 0 c) . zip [0,tileEdge..] .
    map drawCol $ cols

drawCol :: TileCol -> Picture
drawCol (TileCol {colUp = up, colDown = down}) =
    pictures . map (\(i, t) -> translate 0 (fi i) t) $ tiles
  where
    unTileM Nothing  = blank
    unTileM (Just t) = unTile t

    tiles = zip [0,tileEdge..] (map unTileM up) ++
            zip [-tileEdge,-tileEdge*2..] (map unTileM down)

dummyLevel :: Level
dummyLevel = Level { levelBgr  = blank
                   , levelCols = replicate 10 (TileCol [Just tile] [])
                   }
  where
    tile = Tile $ color red $
           polygon [(0,0), (30,0), (30, -30), (0, -30)]