import Test.QuickCheck
import Chapter_03.Ex3_13
import Chapter_03.Ex3_14

import Prelude hiding (min, max)

-----------------------------------------------------------
-- Exercise 3.14

-- | property testing for min and max functions
prop_minMax :: Integer -> Integer -> Bool
prop_minMax x y = (min x y) <= (max x y)

-- | property testing for minThree
prop_min3 :: Integer -> Integer -> Integer -> Bool
prop_min3 x y z = min x (min y z) == minThree x y z

-- | property testing for maxThree
prop_max3 :: Integer -> Integer -> Integer -> Bool
prop_max3 x y z = max x (max y z) == maxThree x y z

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex3-15
ghci> quickCheck prop_minMax
+++ OK, passed 100 tests.
ghci> quickCheck prop_min3
+++ OK, passed 100 tests.
ghci> quickCheck prop_max3
+++ OK, passed 100 tests.

-}