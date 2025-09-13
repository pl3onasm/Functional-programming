module Chapter_06.Ex6_56 where

import Chapter_06.Ex6_53

-----------------------------------------------------------
-- Exercise 6.56

-- | Players in a trick-taking game
data Player = North | East | South | West
  deriving (Show, Eq, Enum, Ord)

-- | A trick consists of a leading player and the cards
-- played in the trick listed in canonical order (i.e.,
-- North, East, South, West)
data Trick = Trick Player [Card]
  deriving (Show, Eq)


-----------------------------------------------------------

{-

This defines a Trick data type that consists of a leading
player and a list of cards played in the trick. Since the
order of play is always clockwise starting from the leader,
we do not need to store which player played which card.
Although the cards in a trick are played starting from the
leader, we store them in canonical order (i.e., North,
East, South, West). The leader is stored explicitly, which
serves to identify the starting point in the stored list of
cards.

Deriving Show allows us to easily print the Trick values,
while deriving Eq allows for equality comparisons between
Trick values.

-}