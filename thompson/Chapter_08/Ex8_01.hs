module Chapter_08.Ex8_01 where

-----------------------------------------------------------
-- Exercise 8.1

data Move = Rock 
          | Paper 
          | Scissors
  deriving (Show,Eq,Ord)

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

outcome :: Move -> Move -> Integer
outcome player1 player2
  | player2 == lose player1 = 1
  | player2 == beat player1 = -1
  | otherwise               = 0


-----------------------------------------------------------

{-

This is just a modification of the outcome function from 
Exercise 4.12.

Testing in GHCi

ghci> :l Ex8_01
ghci> outcome Scissors Paper
1
ghci> outcome Paper Scissors
-1
ghci> outcome Paper Paper
0

-}