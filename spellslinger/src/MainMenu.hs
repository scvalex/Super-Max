module MainMenu (
        -- * State
        State, initState, start,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Data.Char ( digitToInt )
import Data.Monoid ( Monoid(..) )
import Game.Engine ( Game, modifyGameState, getGameState, getsGameState
                   , randomR, upon, getResourceDirectory
                   , Picture(..), TextAlignment(..), Colour(..)
                   , GameEvent(..), Event(..), SDLKey(..), Keysym(..) )
import GlobalCommand ( GlobalCommand(..) )
import Profile ( Profile(..), loadOrNewProfile, saveProfile )
import System.FilePath ( (</>) )
import Text.Printf ( printf )

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
    getResourceDirectory >>= \resDir ->
    loadOrNewProfile `upon` \profile ->
    lookupColourName (resDir </> "rgb.txt") (getProfilePlayerColour profile) `upon` \mcolourName ->
    modifyGameState (\w -> w { getPlayerProfile    = Just profile
                             , getPlayerColourName = mcolourName
                             })

----------------------
-- Callbacks
----------------------

drawState :: State -> Picture
drawState state =
    mconcat $
    [ Translate 0.5 0.80 $ Text 80 CenterAligned "Spellslinger"
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
                mempty
            Just profile ->
                let name = getProfilePlayerName profile
                    headline =
                        case getPlayerColourName state of
                            Nothing -> name
                            Just colName -> printf "%s, the %s wizard" name colName
                in
                Translate 0.5 0.74 $
                mconcat [ Text 40 CenterAligned "featuring"
                        , Translate 0 (-0.08) $
                          Text 60 CenterAligned headline
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
handleEvent (InputEvent (KeyDown (Keysym { symKey = SDLK_r }))) = do
    resDir <- getResourceDirectory
    loadColours (resDir </> "rgb.txt") `upon` \cols -> do
        i <- randomR (0, length cols)
        let (col, colName) = cols !! i
        modifyGameState (\(s@(State { getPlayerProfile = mprofile })) ->
                          case mprofile of
                              Nothing ->
                                  s
                              Just profile ->
                                  s { getPlayerProfile =
                                           Just (profile { getProfilePlayerColour = col })
                                    , getPlayerColourName =
                                           Just colName })
        mprofile <- getsGameState getPlayerProfile
        case mprofile of
            Nothing ->
                return ()
            Just profile ->
                saveProfile profile `upon` \() ->
                    return ()
    return Nothing
handleEvent _ =
    return Nothing

----------------------
-- Flavour text
----------------------

lookupColourName :: FilePath -> Colour -> IO (Maybe String)
lookupColourName colFile col = do
    cols <- loadColours colFile
    return (lookup col cols)

loadColours :: FilePath -> IO [(Colour, String)]
loadColours colFile = do
    text <- readFile colFile
    return (map processLine (lines text))
  where
    processLine :: String -> (Colour, String)
    processLine line =
        let (name, (_:col)) = break (=='\t') line in
        (parseColour col, name)

    parseColour :: String -> Colour
    parseColour ('#':r1:r2:g1:g2:b1:b2:_) =
        let r = digitToInt r1 * 16 + digitToInt r2
            g = digitToInt g1 * 16 + digitToInt g2
            b = digitToInt b1 * 16 + digitToInt b2 in
        RGBA (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
    parseColour col =
        error ("unparsable colour: " ++ col)
