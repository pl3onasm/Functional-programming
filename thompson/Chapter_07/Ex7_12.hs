module Chapter_07.Ex7_12 where

import Prelude hiding (maximum, minimum)
import qualified Prelude (maximum, minimum)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.12

-- | Uses iSort to find the minimum of a list of integers
minimum :: [Integer] -> Integer
minimum xs = head $ iSort xs

-- | Uses iSort to find the maximum of a list of integers
maximum :: [Integer] -> Integer
maximum xs = head $ reverse $ iSort xs

-- | Insertion sort
iSort :: [Integer] -> [Integer]
iSort []       = []
iSort (x : xs) = ins x (iSort xs) 

-- | Inserts an element into a sorted list
ins :: Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y : ys)
  | x <= y    = x : (y : ys)
  | otherwise = y : ins x ys

-- | Property to test if our minimum is equivalent
-- to Prelude.minimum
propMinimum :: [Integer] -> Property
propMinimum xs = 
  not (null xs) ==> minimum xs == Prelude.minimum xs

-- | Property to test if our maximum is equivalent
-- to Prelude.maximum
propMaximum :: [Integer] -> Property
propMaximum xs = 
  not (null xs) ==> maximum xs == Prelude.maximum xs


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :load Ex7_12
ghci> quickCheck propMinimum
+++ OK, passed 100 tests; 16 discarded.
ghci> quickCheck propMaximum
+++ OK, passed 100 tests; 15 discarded.

The discarded tests are cases where the list is empty,
since minimum and maximum are not defined for empty lists.

-}