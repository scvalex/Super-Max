Game Rules
==========

I fear we'll need to find some well-tested rules somewhere and stick
with them.

Orders of magnitude
-------------------

Stat increases are exponential while requirement increases are
polynomial (e.g. it takes 10% more XP to level up again, but your
stats are all doubled; that's the intuition, but we don't use levels;
see "What doesn't kill you..." and "Fighting spirit").

Calculations take into account your HP (you get deal more damage and
and get more XP if you're running low on HP).

There are such things as criticals (^2 damage), super-criticals (^4
damage) and limit breaks (^8 damage). Criticals are fairly common,
super-criticals, less so. Limit breaks can only be done when HP < 5%.

What doesn't kill you...
-----------------------

What doesn't kill you makes you stronger.  For every 1 HP of damage
taken, you gain 1% max HP.  So, your current HP is basically:

    current-hp = starting-hp * 1.01^cumulative-damage-taken

E.g. your HP is multiplied by the following values after so much
damage:

    damage | HP multiplier
    ----------------------
    69     | 2
    139    | 4
    208    | 8
    278    | 16
    348    | 32

Something similar could be done for other "stats"; not sure what those
are, though.

Fighting spirit
---------------

The closer to greeting Death you are, the stronger your will to fight
is and the quicker you get stronger.  The rule of thumb is, if you're
almost dead, you're twice as strong as at full health.

The damage formula is basically:

    damage = normal_damage * (1 + 10 ^ ((max-hp - hp) / max-hp) / 10)

[This formula](http://www.wolframalpha.com/input/?i=plot+%281+%2B+%2810+%5E+x%29%2F10%29+for+x+%3D+0+to+1)
is an exponential increase from 1 to 2, as HP decreases (x is the HP
percentage above).

Something similar should be done for the rate of HP increase.

Team zapping
------------

I am a big fan of the team zapping system used in early Resident Evil
games.

I think each of the main characters should see the world differently.
Alex sees a pessimistic realism.  Max is Kamina, of course. This is
effectively replay levels on different "planes", BUT the repercussions
of character's actions on the world should be felt by the others.
