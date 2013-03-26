module HighScores (
        -- * Game interface
        State, initState, start,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Control.Applicative ( (<$>) )
import Data.Dynamic ( fromDynamic )
import Data.Monoid ( Monoid(..) )
import Game.Engine ( Game, modifyGameState, getGameState, getsGameState
                   , upon, getResource
                   , Picture(..), TextAlignment(..), Colour(..)
                   , GameEvent(..), Event(..), Keysym(..), SDLKey(..) )
import GlobalCommand ( GlobalCommand(..) )
import Text.Printf ( printf )
import Types ( Score )

----------------------
-- Game interface
----------------------

type ScoreEntry = (String, String, Score)

data State = State { getScores :: [ScoreEntry]
                   }

initState :: State
initState = State { getScores = []
                  }

start :: Game State ()
start =
    fetchScores `upon` \scores ->
    modifyGameState (\w -> w { getScores = scores })

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
    drawScore (i, (name, colour, score)) =
        Translate 0.5 (0.5 - (fromIntegral i) * 0.1) $
        Text 50 CenterAligned $
        printf "%s, the %s %20d" name colour score

handleEvent :: GameEvent -> Game State (Maybe GlobalCommand)
handleEvent (InputEvent (KeyUp (Keysym { symKey = SDLK_ESCAPE }))) = do
    return (Just ToMainMenu)
handleEvent _ =
    return Nothing

----------------------
-- Score fetching
----------------------

fetchScores :: IO [ScoreEntry]
fetchScores = return []

----------------------
-- Colours
----------------------

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
