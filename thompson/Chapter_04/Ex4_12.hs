module Chapter_04.Ex4_12 where

-----------------------------------------------------------
-- Exercise 4.12

data Move = Rock 
          | Paper 
          | Scissors
  deriving (Show,Eq)

data Result = Win 
            | Lose 
            | Draw
  deriving (Show,Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

outcome :: Move -> Move -> Result
outcome player1 player2
  | player2 == lose player1 = Win
  | player2 == beat player1 = Lose
  | otherwise               = Draw
  
  
-----------------------------------------------------------

{-

The function beat returns the move that beats the given
move, and the function lose returns the move that loses to
the given move. 

The function outcome compares player2 against player1 to
determine the result from the perspective of player1.
- If player2 plays the move that loses to player1's move,
then player1 wins.
- If player2 plays the move that beats player1's move,
then player1 loses.
- If both players play the same move, the result is a draw.

Testing in GHCi

ghci> :l Ex4_12
ghci> outcome Rock Scissors
Win
ghci> outcome Paper Scissors
Lose
ghci> outcome Paper Paper
Draw

-}