import Chapter_07.Ex7_12 (iSort)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.15

-- | Property to test if iSort produces a permutation of 
-- the input list
propISortPerm :: [Integer] -> Bool
propISortPerm xs = 
  let sorted = iSort xs
  in length xs == length sorted && 
     all (`elem` sorted) xs && all (`elem` xs) sorted


-----------------------------------------------------------

{-

A sorting function should have the following properties:

- the output list is sorted
- the output list is a permutation of the input list 

A permutation means that the output list contains exactly
the same elements as the input list, but possibly in a 
different order.

The propISortPerm property checks the second property. 
It checks that the lengths of the input and output lists
are the same, and that all elements of the input list
are in the output list, and vice versa. The first check
is necessary to ensure that multiplicities of elements
are preserved. Otherwise, the property would be satisfied
by a function that removes duplicates.

Testing in GHCi

ghci> :load Ex7_15
ghci> quickCheck propISortPerm
+++ OK, passed 100 tests.


-}