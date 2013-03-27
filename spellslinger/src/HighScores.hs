{-# LANGUAGE DeriveGeneric #-}

module HighScores (
        -- * Game interface
        State, initState, start,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM )
import Data.Aeson ( FromJSON, decode )
import Data.ByteString.Lazy.Char8 ( pack )
import Data.Dynamic ( fromDynamic )
import Data.Maybe ( fromJust )
import Data.Monoid ( Monoid(..) )
import Game.Engine ( Game, modifyGameState
                   , upon, getResource
                   , Picture(..), TextAlignment(..)
                   , Colour(..), colourFromHexString
                   , GameEvent(..), Event(..), Keysym(..), SDLKey(..) )
import GHC.Generics ( Generic )
import GlobalCommand ( GlobalCommand(..) )
import Network.HTTP ( simpleHTTP, getRequest, getResponseBody )
import Text.Printf ( printf )
import Types ( Score )

----------------------
-- Game interface
----------------------

data ScoreEntry = ScoreEntry { name :: String
                             , colour :: String
                             , score :: Score
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
            mcolName <- lookupColourName (fromJust (colourFromHexString ('#' : colour entry)))
            return $ case mcolName of
                Nothing      -> entry
                Just colName -> entry { colour = colName }

----------------------
-- Callbacks
----------------------

drawState :: State -> Picture
drawState state =
    mconcat $
    [ Translate 0.5 0.80 $ Text 80 CenterAligned "Spellslinger"
    , Translate 0.5 0.74 $ Text 40 CenterAligned "High Scores"
    , mconcat $
      map drawScore (zip [0 :: Int ..] (getScores state))
    ]
  where
    drawScore (i, entry) =
        Translate 0.5 (0.65 - (fromIntegral i) * 0.05) $
        mconcat [ Translate (-0.3) 0.0 $
                  Text 40 LeftAligned $
                  printf "%d. %s, the %s" i (name entry) (colour entry)
                , Translate 0.3 0.0 $
                  Text 40 RightAligned $
                  printf "%d" (score entry)
                ]

handleEvent :: GameEvent -> Game State (Maybe GlobalCommand)
handleEvent (InputEvent (KeyUp (Keysym { symKey = SDLK_ESCAPE }))) = do
    return (Just ToMainMenu)
handleEvent _ =
    return Nothing

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
