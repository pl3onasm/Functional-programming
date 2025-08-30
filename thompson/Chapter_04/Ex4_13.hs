import Test.QuickCheck
import Chapter_04.Ex4_12

-----------------------------------------------------------
-- Exercise 4.13

-- This is the added 'magic' to make QuickCheck work, 
-- enabling it to generate random Move values
instance Arbitrary Move where
  arbitrary = elements [Rock, Paper, Scissors]

-- The property to be tested
prop_inverse :: Move -> Bool
prop_inverse m = 
  ((beat . lose) m == m) && ((lose . beat) m == m)


-----------------------------------------------------------

{-

This property checks the inverse relationship between the 
functions beat and lose. It checks if applying beat and 
lose in sequence cancels out and returns the original move.

QuickCheck will generate all possible Move values
and test that the property holds for each of them.

Testing in GHCi

ghci> :set -i..
ghci> :l Ex4_13
ghci> quickCheck prop_inverse
+++ OK, passed 100 tests.

-}