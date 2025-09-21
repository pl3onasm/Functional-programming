import Chapter_08.Ex8_01

-----------------------------------------------------------
-- Exercise 8.5

-- | A strategy is a function that takes a list of the
-- opponent's previous moves and returns the next move.
type Strategy = [Move] -> Move

-- | Counts how many times a Move appears in a 
-- list of Moves
count :: Move -> [Move] -> Integer
count _ [] = 0
count x (m : ms)
  | x == m    = 1 + count x ms
  | otherwise = count x ms

-- | Returns the most frequently played Move
mostFrequent :: [Move] -> Move
mostFrequent history
  | r >= p && r >= s = Rock
  | p >= r && p >= s = Paper
  | otherwise        = Scissors
  where
    r = count Rock history
    p = count Paper history
    s = count Scissors history

-- | Given a Move, returns a Move that beats at least
-- one of the other two Moves.
safeMove :: Move -> Move
safeMove Rock     = Scissors
safeMove Paper    = Rock
safeMove Scissors = Paper

-- | A strategy that plays the move that beats at least
-- one of the opponent's least frequently played moves.
-- If there is no history, it plays Rock.
freqStrategy :: Strategy
freqStrategy [] = Rock
freqStrategy history = safeMove (mostFrequent history)


-----------------------------------------------------------

{-

This strategy counts how many times each move has been
played by the opponent, determines the most frequently 
played move, and then chooses a move that beats at least 
one of the other two moves (i.e., the moves that are not 
the most frequent).
The same function safeMove is used as in Exercise 8.4 to 
choose a move that guarantees to beat at least one of the
other two moves, the least frequent ones in this case.

Note that in the case of a tie for the most frequent move,
the first in the guard order is chosen. This means that
if Rock and Paper are tied for most frequent, Rock will
be chosen. 

Testing in GHCi

ghci> :set -i..
ghci> :l Ex8_05
ghci> freqStrategy []
Rock
ghci> freqStrategy [Rock,Paper,Rock,Rock]
Rock
ghci> freqStrategy [Scissors,Scissors,Paper]
Paper


-}