module GlobalCommand (
        GlobalCommand(..)
    ) where

----------------------
-- Global commands
----------------------

data GlobalCommand = ToContinue
                   | ToNewGame
                   | ToMainMenu
                   | ToQuit
