import Chapter_08.Ex8_01

-----------------------------------------------------------
-- Exercise 8.3

-- | A strategy is a function that takes a list of the
-- opponent's previous moves and returns the next move.
type Strategy = [Move] -> Move

-- | A strategy playing the move that beats the opponent's 
-- last move. If there is no last move, it plays Rock.
beatLast :: Strategy
beatLast []      = Rock
beatLast (m : _) = beat m

-- | A strategy playing the move that loses to the 
-- opponent's last move. If there is no last move, 
-- it plays Rock.
loseLast :: Strategy
loseLast []      = Rock
loseLast (m : _) = lose m


-----------------------------------------------------------

{-

What the better strategy is depends on what the opponent 
is doing. If the opponent is playing a random sequence of 
moves, then both strategies will do equally well. If the
opponent is playing a non-random sequence of moves, then
the strategy that beats the opponent's last move will do
better, because it will win against any move that the
opponent plays twice in a row. This is what people tend
to do when playing rock-paper-scissors, either because
they are not good at generating random sequences, or
because they think that their opponent expects them to
switch moves, and so they try to outsmart their opponent
by repeating a move.


Testing in GHCi

ghci> :l Ex8_03
ghci> beatLast []
Rock
ghci> beatLast [Rock,Paper,Scissors]
Paper
ghci> loseLast []
Rock
ghci> loseLast [Rock,Paper,Scissors]
Scissors


-}