module Profile (
       Profile(..), loadOrNewProfile, saveProfile
    ) where

import Common ( writeAppFile )
import Game.Engine ( Game, Colour(..)
                   , upon )
import System.Posix.User ( getLoginName )

data Profile = Profile { getPlayerName   :: String
                       , getPlayerColour :: Colour
                       } deriving ( Show, Read )

loadOrNewProfile :: (Profile -> Game a ()) -> Game a ()
loadOrNewProfile handler = do
    newSaveProfile `upon` handler
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

-- | Save a profile to disk.
saveProfile :: Profile -> IO ()
saveProfile profile = do
    writeAppFile "profile" (show profile)
