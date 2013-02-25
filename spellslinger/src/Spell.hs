-- | Magic craft is governed by strict rules.
module Spell (
        Word(..), Spell, Effect, names, spellEffect
    ) where

import Types ( Direction(..) )

-- | A spell is just a string of words of power.  Although all strings of words of power
-- are spells, most are uninteresting.
type Spell = [Word]

-- | Some spells have effects.  For instance, they may create fire.
data Effect = Fire | Jump
            deriving ( Eq, Show )

-- | A word of power informs the Universe that it is wrong, and that it should change
-- itself accordingly.
data Word = Quote
          | Repeat
          | Move Direction
          | Effect Effect
          deriving ( Eq, Show )

-- | There are two naming rules.  These are used by the other rules.
--
-- Rule Q: The spell "Qx" names "x".
--
-- Rule R: If "y" names "x", the spell "Ry" names "xx".
names :: Spell -> Maybe Spell
names (Quote : x) =
    Just x
names (Repeat : y) = do
    x <- names y
    return (x ++ x)
names _ =
    Nothing

-- | There is one rule governing the effects of spell.
--
-- Rule E: If "x" names "dey", the spell "x" moves in direction "d", has effect "e", and
-- casts spell "y".
spellEffect :: Spell -> Maybe (Direction, Effect, Spell)
spellEffect x = do
    dey <- names x
    case dey of
        (Move d : Effect e : y) -> return (d, e, y)
        _                       -> Nothing
