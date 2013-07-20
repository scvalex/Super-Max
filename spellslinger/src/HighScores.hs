{-# LANGUAGE DeriveGeneric #-}

module HighScores (
        -- * Game interface
        State, initState, start,

        -- * Callbacks
        drawState, handleInput, handleTick
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM )
import Data.Aeson ( FromJSON, decode )
import Data.ByteString.Lazy.Char8 ( pack )
import Data.Default ( def )
import Data.Dynamic ( fromDynamic )
import Data.Maybe ( fromJust )
import GHC.Generics ( Generic )
import GlobalCommand ( GlobalCommand(..) )
import Network.HTTP ( simpleHTTP, getRequest, getResponseBody )
import SuperMax ( Game, modifyGameState
                , upon, getResource
                , Drawable(..), Drawing(..), SomeDrawable(..), Text(..)
                , Colour(..), fromHexString
                , InputEvent(..), KeyEvent(..), Key(..) )
import Types ( Score )
import Text.Printf ( printf )

----------------------
-- Game interface
----------------------

data ScoreEntry = ScoreEntry { name   :: String
                             , colour :: String
                             , score  :: Score
                             } deriving ( Generic )

instance FromJSON ScoreEntry

data State = State { getScores :: [ScoreEntry]
                   }

initState :: State
initState = State { getScores = []
                  }

start :: Game State ()
start =
    fetchScores `upon` \scores -> do
        scores' <- replaceColourNames scores
        modifyGameState (\w -> w { getScores = scores' })
  where
    replaceColourNames scores = do
        forM scores $ \entry -> do
            mcolName <- lookupColourName (fromJust (fromHexString ('#' : colour entry)))
            return $ case mcolName of
                Nothing      -> entry
                Just colName -> entry { colour = colName }

----------------------
-- Callbacks
----------------------

drawState :: State -> Drawing
drawState state = def { drawingDrawables = [SomeDrawable state] }

-- FIXME Why does going to highscores sometimes kill the game silently?
instance Drawable State where
    drawableHudTexts state = concat
        [ [ Text { textFontName = "holstein"
                 , textPosition = (-0.8, 0.5)
                 , textSize     = 0.14
                 , textText     = "Highscores"
                 }
          ]
        , [ Text { textFontName = "holstein"
                 , textPosition = (-0.9, 0.3 - (fromIntegral i * 0.12))
                 , textSize     = 0.06
                 , textText     = printf "%d. %s, the %s %6d"
                                         i (name entry) (colour entry) (score entry)
                 }
          | (i, entry) <- zip [0 :: Int ..] (getScores state) ]
        ]

handleInput :: InputEvent -> Game State (Maybe GlobalCommand)
-- FIXME Why does pressing escape kill the game?
handleInput (InputEvent (KeyRelease KeyEsc)) = return (Just ToMainMenu)
handleInput _                                = return Nothing

handleTick :: Float -> Game State (Maybe GlobalCommand)
handleTick _ = return Nothing

----------------------
-- Score fetching
----------------------

fetchScores :: IO [ScoreEntry]
fetchScores = do
    text <- getResponseBody =<<
            Network.HTTP.simpleHTTP (getRequest "http://abstractbinary.org/spellslinger/scores")
    case decode (pack text) of
        Nothing ->
            return []
        Just entries -> do
            return entries

----------------------
-- Colours
----------------------

-- FIXME Colours handling is duplicated in MainMenu and HighScores.
-- | The name of the colours resource.
coloursResName :: String
coloursResName = "MainMenu.colours"

-- | Lookup the name of a 'Colour'.
lookupColourName :: Colour -> Game State (Maybe String)
lookupColourName col = lookup col <$> getColours

-- | Retrieve the map of colours from the game state.
getColours :: Game State [(Colour, String)]
getColours = do
    mcols <- getResource coloursResName
    let Just cols = fromDynamic =<< mcols
    return cols
