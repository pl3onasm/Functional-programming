import Chapter_07.Ex7_12 (iSort, ins)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.14

-- | Checks if a list is sorted in ascending order
isSorted :: [Integer] -> Bool
isSorted []  = True
isSorted [_] = True
isSorted (x : y : zs) = (x <= y) && isSorted (y : zs)

-- | Property to test if iSort produces a sorted list
propISort :: [Integer] -> Bool
propISort xs = isSorted (iSort xs)

-- | Property to test if ins maintains sorted order
propIns :: Integer -> [Integer] -> Property
propIns x ys = isSorted ys ==> isSorted (ins x ys)

-- | Rewrite of propIns to avoid too many discarded tests
propIns' :: Integer -> [Integer] -> Bool
propIns' x ys = isSorted (ins x (iSort ys))


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :load Ex7_17
ghci> quickCheck propISort
+++ OK, passed 100 tests.
ghci> quickCheck propIns
*** Gave up! Passed only 72 tests; 1000 discarded tests.
ghci> quickCheck propIns'
+++ OK, passed 100 tests.

The propIns property discards many tests because QuickCheck
is generating many unsorted lists for ys, which do not 
satisfy the precondition. That is why it gives up after
only 72 successful tests out of 1000 attempts.
The propIns' property avoids this by sorting ys first. Now
the precondition is always satisfied, and all tests are 
successful.


-}