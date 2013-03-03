module MainMenu (
        -- * State
        State, initState, start,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Data.Monoid ( Monoid(..) )
import Game.Engine ( Game, modifyGameState, getGameState, upon
                   , Picture(..), TextAlignment(..), Colour(..)
                   , GameEvent(..), Event(..), SDLKey(..), Keysym(..) )
import GlobalCommand ( GlobalCommand(..) )
import Profile ( Profile(..), loadOrNewProfile )

----------------------
-- State
----------------------

data State = State { getItems            :: [(String, GlobalCommand)]
                   , getSelectedItem     :: Int
                   , getPlayerProfile    :: Maybe Profile
                   , getPlayerColourName :: Maybe String
                   }

initState :: State
initState = State { getItems = [ ("Continue", ToContinue)
                               , ("New Game", ToNewGame)
                               , ("Quit", ToQuit)
                               ]
                  , getSelectedItem     = 0
                  , getPlayerProfile    = Nothing
                  , getPlayerColourName = Nothing
                  }

start :: Game State ()
start =
    loadOrNewProfile $ \profile ->
    lookupColourName (getPlayerColour profile) `upon` \mcolourName ->
    modifyGameState (\w -> w { getPlayerProfile = Just profile
                             , getPlayerColourName = mcolourName
                             })

----------------------
-- Callbacks
----------------------

drawState :: State -> Picture
drawState state =
    mconcat $
    [ Translate 0.5 0.75 $ Text 80 CenterAligned "Spellslinger"
    , featuring
    , mconcat $
      map drawMenuItem (zip [0 :: Int ..] (map fst (getItems state)))
    ]
  where
    drawMenuItem (i, text) = Translate 0.5 (0.5 - (fromIntegral i) * 0.1) $
                             Text 50 CenterAligned $
                             if i == getSelectedItem state
                             then "- " ++ text ++ " -"
                             else text

    featuring =
        case getPlayerProfile state of
            Nothing ->
                -- mempty
                Translate 0.5 0.7 $
                Text 40 CenterAligned "no profile"
            Just profile ->
                Translate 0.5 0.7 $
                mconcat [ Text 40 CenterAligned "featuring"
                        , Translate 0 (-0.05) $
                          Text 60 CenterAligned (getPlayerName profile)
                        ]

handleEvent :: GameEvent -> Game State (Maybe GlobalCommand)
handleEvent (InputEvent (KeyDown (Keysym { symKey = SDLK_ESCAPE }))) = do
    return (Just ToQuit)
handleEvent (InputEvent (KeyDown (Keysym { symKey = SDLK_UP }))) = do
    modifyGameState (\s -> s { getSelectedItem = max 0 (getSelectedItem s - 1) })
    return Nothing
handleEvent (InputEvent (KeyDown (Keysym { symKey = SDLK_DOWN }))) = do
    modifyGameState (\s -> s { getSelectedItem = min (length (getItems s) - 1)
                                                     (getSelectedItem s + 1) })
    return Nothing
handleEvent (InputEvent (KeyDown (Keysym { symKey = key })))
    | key `elem` [SDLK_SPACE, SDLK_RETURN] = do
        state <- getGameState
        -- The index can't be wrong, unless we screwed up updating getSelectedItem.
        return (Just (snd (getItems state !! getSelectedItem state)))
handleEvent _ =
    return Nothing

----------------------
-- Flavour text
----------------------

lookupColourName :: Colour -> IO (Maybe String)
lookupColourName _ =
    return Nothing
