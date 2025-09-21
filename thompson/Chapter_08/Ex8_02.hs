import Chapter_08.Ex8_01

-----------------------------------------------------------
-- Exercise 8.2

tournamentOutcome :: [Move] -> [Move] -> Integer
tournamentOutcome moves1 moves2 = 
  sum [outcome p1 p2 | (p1,p2) <- zip moves1 moves2]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex8_02
ghci> player1 = [Rock, Rock, Paper]
ghci> player2 = [Scissors, Paper, Rock]
ghci> tournamentOutcome player1 player2
1

-}