import Test.QuickCheck
import Chapter_04.Ex4_12

-----------------------------------------------------------
-- Exercise 4.14

-- This is the added 'magic' to make QuickCheck work, 
-- enabling it to generate random Move values
instance Arbitrary Move where
  arbitrary = elements [Rock, Paper, Scissors]

-- The property tests the symmetry of the outcome function
prop_outcome :: Move -> Move -> Bool
prop_outcome player1 player2 =
  case outcome player1 player2 of
    Win  -> outcome player2 player1 == Lose
    Lose -> outcome player2 player1 == Win
    Draw -> outcome player2 player1 == Draw


-----------------------------------------------------------

{-

This property checks the symmetry of the outcome function.
It verifies that if player1 wins against player2, then
player2 must lose against player1, and vice versa. If the
result is a draw, it should be a draw from both 
perspectives.

QuickCheck will generate all possible pairs of Move
values and test that the property holds for each pair.

Testing in GHCi

ghci> :set -i.. 
ghci> :l Ex4_14
ghci> quickCheck prop_outcome
+++ OK, passed 100 tests.

-}