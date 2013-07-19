module MainMenu (
        -- * Game interface
        State, initState, start, loadResources,

        -- * Callbacks
        drawState, handleInput, handleTick
    ) where

import Control.Applicative ( (<$>) )
import Data.Default ( def )
import Data.Dynamic ( Dynamic, toDyn, fromDynamic )
import Data.Map ( Map )
import Data.Maybe ( fromJust )
import GlobalCommand ( GlobalCommand(..) )
import Profile ( Profile(..), loadOrNewProfile, saveProfile )
import qualified Data.Map as M
import SuperMax ( Game, modifyGameState, getGameState, getsGameState
                , randomR, upon, getResource
                , SomeDrawable(..), Drawable(..), Drawing(..), Text(..)
                , Colour(..), fromHexString
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

drawState :: State -> Drawing
drawState state = def { drawingDrawables = [SomeDrawable state] }

instance Drawable State where
    drawableHudTexts state = concat
        [ [ Text { textFontName = "holstein"
                 , textPosition = (-0.8, 0.5)
                 , textSize     = 0.14
                 , textText     = "Spellslinger"
                 }
          , Text { textFontName = "holstein"
                 , textPosition = (-0.6, 0.4)
                 , textSize     = 0.06
                 , textText     = "featuring"
                 }
          , Text { textFontName = "holstein"
                 , textPosition = (-0.6, 0.3)
                 , textSize     = 0.08
                 , textText     = featuring
                 }
          ]
        , [ Text { textFontName = "holstein"
                 , textPosition = (-0.6, 0.0 - 0.2 * (fromIntegral i))
                 , textSize     = 0.1
                 , textText     = if i == getSelectedItem state
                                  then "> " ++ text
                                  else "  " ++ text
                 }
          | (i, (text, _)) <- zip [0 :: Int ..] (getItems state) ]
        ]
      where
        featuring =
            case getPlayerProfile state of
                Nothing ->
                    ""
                Just profile ->
                    let name = getProfilePlayerName profile
                    in case getPlayerColourName state of
                        Nothing -> name
                        Just colName -> printf "%s, the %s wizard" name colName

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
        (fromJust (fromHexString col), name)
