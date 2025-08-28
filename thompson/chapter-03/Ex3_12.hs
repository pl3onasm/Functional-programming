import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 3.12

-- | checks if three integers are equal
threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual x y z = (x == y) && (y == z)

-- | checks if at least one pair of integers differs
mystery :: Integer -> Integer -> Integer -> Bool
mystery x y z = not ((x == y) && (y == z))

-- | comparing mystery and not . threeEqual
prop_mystery :: Integer -> Integer -> Integer -> Bool
prop_mystery x y z = mystery x y z 
                     == not (threeEqual x y z)

-- | checks if three integers are pairwise different
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = (x /= y) && (y /= z) && (x /= z)

-- | comparing threeDifferent and not . threeEqual
prop_3Diff :: Integer -> Integer -> Integer -> Bool
prop_3Diff x y z = threeDifferent x y z
                   == not (threeEqual x y z)

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :load Ex3-12
ghci> quickCheck prop_mystery
+++ OK, passed 100 tests.
ghci> quickCheck prop_3Diff
*** Failed! Falsified (after 2 tests):
1
0
0

The counterexample from quickCheck is:
  x = 1
  y = 0
  z = 0

We will check this manually:

  threeDifferent 1 0 0
=   { applying threeDifferent }
  (1 /= 0) && (0 /= 0) && (1 /= 0)
=   { evaluating the comparisons }
  True && False && True
=   { evaluating the first && }
  False && True
=   { evaluating the remaining && }
  False

  not (threeEqual 1 0 0)
=   { applying threeEqual }
  not ((1 == 0) && (0 == 0))
=   { evaluating the comparisons }
  not (False && True)
=   { evaluating && }
  not False
=   { evaluating not }
  True

We see that the two functions give different results for 
the counterexample (1,0,0). So the property is false,
and the two functions are not equivalent.
ThreeDifferent check if all three arguments are pairwise
different, while not . threeEqual only checks that not all
three arguments are the same.

-}