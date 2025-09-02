import Test.QuickCheck
import Chapter_04.Ex4_32

-----------------------------------------------------------
-- Exercise 4.39

-- property to test the power2 function
prop_power2 :: NonNegative Integer -> Bool
prop_power2 (NonNegative n) = power2 n == 2 ^ n


-----------------------------------------------------------

{-

This property checks that the power2 function computes
the same result as the built-in exponentiation operator (^)
for non-negative integers.
The type NonNegative Integer is used to ensure that the
input to power2 is always a non-negative integer. It is
provided by the QuickCheck library.

Testing in GHCi

ghci> :set -i..
ghci> :load Ex4_39.hs
ghci> quickCheck prop_power2
+++ OK, passed 100 tests.

-}