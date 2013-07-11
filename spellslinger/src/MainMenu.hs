module MainMenu (
        -- * Game interface
        State, initState, start, loadResources,

        -- * Callbacks
        drawState, handleInput, handleTick
    ) where

import Control.Applicative ( (<$>) )
import Data.Dynamic ( Dynamic, toDyn, fromDynamic )
import Data.Map ( Map )
import Data.Maybe ( fromJust )
import Data.Monoid ( Monoid(..) )
import GlobalCommand ( GlobalCommand(..) )
import Profile ( Profile(..), loadOrNewProfile, saveProfile )
import qualified Data.Map as M
import SuperMax ( Game, modifyGameState, getGameState, getsGameState
                , randomR, upon, getResource
                , Picture(..), TextAlignment(..)
                , Colour(..), colourFromHexString
                , InputEvent(..), KeyEvent(..), Key(..) )
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
                               , ("High Scores", ToHighScores)
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

handleInput :: InputEvent -> Game State (Maybe GlobalCommand)
handleInput (InputEvent (KeyPress KeyEsc)) = do
    return (Just ToQuit)
handleInput (InputEvent (KeyPress KeyUp)) = do
    modifyGameState (\s -> s { getSelectedItem = max 0 (getSelectedItem s - 1) })
    return Nothing
handleInput (InputEvent (KeyPress KeyDown)) = do
    modifyGameState (\s -> s { getSelectedItem = min (length (getItems s) - 1)
                                                     (getSelectedItem s + 1) })
    return Nothing
handleInput (InputEvent (KeyPress key))
    | key `elem` [KeySpace, KeyEnter] = do
        state <- getGameState
        -- The index can't be wrong, unless we screwed up updating getSelectedItem.
        return (Just (snd (getItems state !! getSelectedItem state)))
handleInput (InputEvent (KeyPress (CharKey 'r'))) = do
    cols <- getColours
    i <- randomR (0, length cols - 1)
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
handleInput _ =
    return Nothing

handleTick :: Float -> Game State (Maybe GlobalCommand)
handleTick _ = return Nothing

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
        (fromJust (colourFromHexString col), name)
