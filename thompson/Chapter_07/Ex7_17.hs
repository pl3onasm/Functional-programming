import Chapter_07.Ex7_16
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.17

isSorted1 :: [Integer] -> Bool
isSorted1 []       = True
isSorted1 [_]      = True
isSorted1 (x:y:xs) = (x >= y) && isSorted1 (y:xs)

isSorted2 :: [Integer] -> Bool
isSorted2 []       = True
isSorted2 [_]      = True
isSorted2 (x:y:xs) = (x < y) && isSorted2 (y:xs)

-- | Property to test if iSort1 produces a descending 
-- sorted list
propISort1 :: [Integer] -> Bool
propISort1 xs = isSorted1 (iSort1 xs)

-- | Property to test if iSort2 produces a sorted list
-- without duplicates (strictly increasing order)
propISort2 :: [Integer] -> Bool
propISort2 xs = isSorted2 (iSort2 xs)


-----------------------------------------------------------

{-

The isSorted function needs to be modified to check for the
correct order based on the sorting criteria. For iSort1, 
isSorted1 checks for descending order (x >= y), while for
iSort2, isSorted2 checks for strictly increasing order 
(x < y).


Testing in GHCi

ghci> :load Ex7_17
ghci> quickCheck propISort1
+++ OK, passed 100 tests.
ghci> quickCheck propISort2
+++ OK, passed 100 tests.

-}