module Chapter_08.Ex8_06 where

import Chapter_08.Ex8_01
import Chapter_08.Ex8_04

-----------------------------------------------------------
-- Exercise 8.6

-- | Finds the two most recent distinct moves in the
-- opponent's history and returns the absent move.
-- If there are fewer than two distinct moves, returns
-- a pseudo-random move based on the length of the history.
leastRecent :: [Move] -> Move
leastRecent history = check [] history 
  where
  check [m1,m2] _ = absent [m1,m2]
  check [m] []    = (others m) !! (length history `mod` 2)
  check [] []     = error "leastRecent: empty history"
  check seen (m : ms)
    | m `elem` seen = check seen ms
    | otherwise     = check (m : seen) ms

-- | Given a list of two Moves, 
-- returns the one that is absent.
absent :: [Move] -> Move
absent seen
  | Rock  `notElem` seen    = Rock
  | Paper `notElem` seen    = Paper
  | otherwise               = Scissors

-- | Given a Move, returns a list of the other two Moves.
others :: Move -> [Move]
others Rock     = [Paper,Scissors]
others Paper    = [Rock,Scissors]
others Scissors = [Rock,Paper]

-- | Strategy that plays the move that beats the one the
-- opponent has not played most recently.
beatLeastRecent :: Strategy
beatLeastRecent []       = randomMove ()
beatLeastRecent [m]      = safeMove m
beatLeastRecent history  = beat (leastRecent history)

-- | Strategy that predicts the opponent's next move will
-- be the one that would have lost to their previous move,
-- and plays the move that beats that predicted move.
beatLostLast :: Strategy
beatLostLast []      = randomMove ()
beatLostLast (m : _) = beat (lose m)

-- | Strategy that predicts the opponent's next move will
-- be the one that would have beaten their previous move,
-- and plays the move that beats that predicted move.
beatWonLast :: Strategy
beatWonLast []      = randomMove ()
beatWonLast (m : _) = beat (beat m)


-----------------------------------------------------------

{-

In this exercise, we have implemented three more 
strategies.

The first strategy, beatLeastRecent, finds the two most
recent *distinct* moves in the opponent's history and 
returns the absent move, which is the one that the opponent 
has not played most recently. In case the history has fewer 
than two distinct moves, we pseudo-randomly pick a move 
from the two absent ones. The strategy then plays the move 
that beats the returned move. This strategy assumes that 
the opponent wants to play all three moves with roughly 
equal frequency, and so it predicts that the opponent will 
play the move that they have not played for the longest 
time.

The second strategy, beatLostLast, predicts that the
opponent's next move will be the one that would have lost
to their previous move, and plays the move that beats that
predicted move. If there is no previous move, it plays a
random move. This strategy actually assumes that the 
opponent will rotate backwards

The third strategy, beatWonLast, predicts that the 
opponent's next move will be the one that would have beaten
their previous move, and plays the move that beats that 
predicted move. If there is no previous move, it plays a random 
move. This strategy actually assumes that the opponent will 
rotate forwards.

Testing in GHCi

ghci> :set -i..
ghci> :set -package random
ghci> :l Ex8_06
ghci> beatLeastRecent [Rock, Paper, Rock, Rock]
Rock
ghci> beatLeastRecent [Rock, Paper, Scissors, Rock]
Rock
ghci> beatLeastRecent [Rock, Rock, Rock, Rock]
Scissors
ghci> beatLostLast [Paper, Scissors, Rock]
Paper
ghci> beatWonLast [Paper, Scissors, Rock]
Rock

-}