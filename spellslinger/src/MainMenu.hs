module MainMenu (
        -- * Game interface
        State, initState, start, loadResources,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Control.Applicative ( (<$>) )
import Data.Char ( digitToInt )
import Data.Dynamic ( Dynamic, toDyn, fromDynamic )
import Data.Map ( Map )
import Data.Monoid ( Monoid(..) )
import Game.Engine ( Game, modifyGameState, getGameState, getsGameState
                   , randomR, upon, getResource
                   , Picture(..), TextAlignment(..), Colour(..)
                   , GameEvent(..), Event(..), SDLKey(..), Keysym(..) )
import GlobalCommand ( GlobalCommand(..) )
import Profile ( Profile(..), loadOrNewProfile, saveProfile )
import qualified Data.Map as M
import System.FilePath ( (</>) )
import Text.Printf ( printf )

----------------------
-- Game interface
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
    loadOrNewProfile `upon` \profile -> do
        mcolourName <- lookupColourName (getProfilePlayerColour profile)
        modifyGameState (\w -> w { getPlayerProfile    = Just profile
                                 , getPlayerColourName = mcolourName
                                 })

loadResources :: FilePath -> IO (Map String Dynamic)
loadResources resDir = do
    colours <- loadColours (resDir </> "rgb.txt")
    return (M.insert coloursResName (toDyn colours) M.empty)

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
    cols <- getColours
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

-- | Load colours from disk.
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
