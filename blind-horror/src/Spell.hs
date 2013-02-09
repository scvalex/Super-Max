module Spell (
        Word(..), Spell, names
    ) where

import Types ( Direction(..) )

data Word = Quote
          | Repeat
          | Move Direction
          | Fire
          deriving ( Eq, Show )

type Spell = [Word]

names :: Spell -> Maybe Spell
names (Quote : ws) =
    Just ws
names (Repeat : ws) = do
    ws' <- names ws
    return (ws' ++ ws')
names _ =
    Nothing
