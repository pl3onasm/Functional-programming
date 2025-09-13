module Chapter_06.Ex6_60 where

import Chapter_06.Ex6_53
import Chapter_06.Ex6_59

-----------------------------------------------------------
-- Exercise 6.60

-- | Hands held by all four players, ordered in 
-- canonical order (North, East, South, West)
type Hands = [Hand]


-----------------------------------------------------------

{-

We chose to represent a collection of hands held by the 
four players as a list of hands, ordered in canonical
order (North, East, South, West).
This way, the structure is in line with the Trick type, 
which also stores cards in canonical order, and it is easy
to associate each hand with the corresponding player. We
can also zip the list of hands with a list of players
[North .. West] to get pairs of players and their hands.


-}