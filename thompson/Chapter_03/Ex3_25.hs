import Test.QuickCheck
import Chapter_03.Ex3_23
import Chapter_03.Ex3_24

-----------------------------------------------------------
-- Exercise 3.25

-- | Property 1: if there are two roots, the smaller root 
-- is less than the larger root
prop_smaller :: Float -> Float -> Float -> Bool
prop_smaller a b c
  | nRoots == 2 = smallerRoot a b c <= largerRoot a b c
  | otherwise   = True
    where nRoots = numberRoots a b c

-- | Property 2: if there are two roots, they satisfy the
-- equation up to a some small rounding error
prop_satEq :: Float -> Float -> Float -> Bool
prop_satEq a b c
  | nRoots == 2 = abs (a*x^2 + b*x + c) < epsilon &&
                  abs (a*y^2 + b*y + c) < epsilon
  | otherwise   = True
    where 
      nRoots = numberRoots a b c
      x = smallerRoot a b c
      y = largerRoot a b c
      epsilon = 1e-2

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex3-25
ghci> quickCheck prop_smaller
+++ OK, passed 100 tests.
ghci> quickCheck prop_satEq
+++ OK, passed 100 tests.

-}