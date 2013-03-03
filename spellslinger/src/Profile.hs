module Profile (
       Profile(..), loadOrNewProfile, saveProfile, loadProfile
    ) where

import Common ( writeAppFile, readAppFile )
import Control.Applicative ( (<$>) )
import Game.Engine ( Game, Colour(..)
                   , upon )
import System.Posix.User ( getLoginName )

data Profile = Profile { getPlayerName   :: String
                       , getPlayerColour :: Colour
                       } deriving ( Show, Read )

loadOrNewProfile :: (Profile -> Game a ()) -> Game a ()
loadOrNewProfile handler = do
    go `upon` handler
  where
    go = do
        mprofile <- loadProfile
        case mprofile of
            Just profile -> return profile
            Nothing      -> newSaveProfile

    newSaveProfile = do
        profile <- newProfile
        saveProfile profile
        return profile

    newProfile = do
        username <- getLoginName
        return (Profile { getPlayerName = username
                        , getPlayerColour = RGBA 255 140 0 255
                        })

-- | Save the user profile to disk.
saveProfile :: Profile -> IO ()
saveProfile profile = do
    writeAppFile "profile" (show profile)

-- | Read the user profile from disk.
loadProfile :: IO (Maybe Profile)
loadProfile = do
    text <- readAppFile "profile"
    return (read <$> text)