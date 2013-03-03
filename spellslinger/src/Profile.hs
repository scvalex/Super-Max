module Profile (
       Profile(..), loadOrNewProfile, saveProfile, loadProfile
    ) where

import Common ( writeAppFile, readAppFile )
import Control.Applicative ( (<$>) )
import Game.Engine ( Colour(..) )
import System.Posix.User ( getLoginName )

data Profile = Profile { getPlayerName   :: String
                       , getPlayerColour :: Colour
                       } deriving ( Show, Read )

-- | Load a profile from disk, if it exists, or generate a new one.
loadOrNewProfile :: IO Profile
loadOrNewProfile = do
    mprofile <- loadProfile
    case mprofile of
        Just profile -> return profile
        Nothing      -> newSaveProfile
  where
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
