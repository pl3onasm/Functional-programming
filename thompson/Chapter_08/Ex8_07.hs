import Chapter_08.Ex8_01
import Chapter_08.Ex8_04
import Chapter_08.Ex8_06

-----------------------------------------------------------
-- Exercise 8.7

-- | Strategy that alternates between two strategies.
alternate :: Strategy -> Strategy -> Strategy
alternate strat1 strat2 history 
  | even (length history) = strat1 history
  | otherwise             = strat2 history


-----------------------------------------------------------

{- 

The function alternate takes two strategies, s1 and s2, and
returns a new strategy that alternates between them on each 
turn. The alternation is determined by the length of the 
opponent's history: if the number of previous moves is 
even, it uses s1, otherwise s2.

Subtlety: alternate itself only takes two arguments (the 
strategies). The resulting strategy is a function of type 
[Move] -> Move, so when we later apply it to the opponent's 
move history, that history is the 'third' argument in 
effect. This can be a little confusing, but in Haskell this 
is just normal function application: the returned strategy 
is itself a function waiting for the history.

Example usage:

ghci> strat = alternate beatLostLast beatWonLast
ghci> strat [Rock, Paper, Rock, Scissors]
Rock   
ghci> strat [Scissors, Rock, Paper, Rock, Scissors]
Paper

Note on the examples: 

In the first example, the length of the history is 4, which
is even, so beatLostLast is used. The opponent's most 
recent move was Rock (head of the list), and so we predict 
that their next move will be Scissors (the move that would 
have lost to Rock), and so we play Rock (the move that 
beats Scissors).

In the second example, the length of the history is 5, 
which is odd, so beatWonLast is used. The opponent's
most recent move was Scissors, and so we predict that their
next move will be Rock (the move that would have beaten
Scissors), and so we play Paper (the move that beats Rock).

-}