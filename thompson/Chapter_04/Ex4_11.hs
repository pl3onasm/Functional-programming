-----------------------------------------------------------
-- Exercise 4.11

data Move = Rock 
          | Paper 
          | Scissors
  deriving (Show,Eq)

data Result = Win 
            | Lose 
            | Draw
  deriving (Show,Eq)
  

-----------------------------------------------------------

{-

The extra data type Result is defined to represent the 
outcome of a game from the perspective of a player.

-}