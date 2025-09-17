import Test.QuickCheck
import Data.List (sort)

-----------------------------------------------------------
-- Exercise 7.19

-- | Comparison function for pairs of integers 
-- (lexicographic order)
leq :: (Integer,Integer) -> (Integer,Integer) -> Bool
leq (v, x) (y, z) = (v < y) || (v == y && x <= z)

-- | Inserts a pair into a sorted list of pairs of integers
insert :: (Integer,Integer) -> [(Integer,Integer)] 
            -> [(Integer,Integer)]
insert p [] = [p]
insert p (q : qs)
  | leq p q   = p : (q : qs)
  | otherwise = q : insert p qs

-- | Insertion sort for a list of pairs of integers 
iSort :: [(Integer,Integer)] -> [(Integer,Integer)]
iSort []       = []
iSort (p : ps) = insert p (iSort ps)

-- | Property: iSort should produce a 
-- lexicographically sorted list of pairs
propISort :: [(Integer,Integer)] -> Bool
propISort xs = iSort xs == sort xs


-----------------------------------------------------------

{-

We defined a helper function leq that compares two pairs of
integers in lexicographic order, but this is not strictly
necessary since the standard comparison operators for 
pairs already do this. So we could have replaced leq p q
with p <= q in the insert function.

Also, we used the built-in sort function from Data.List
to define the property propISort, since it sorts lists
of pairs in lexicographic order by default. By comparing
the output of our iSort function with the output of sort,
we can verify the two essential properties for a sorting
function: that that the output is sorted and that it is 
a permutation of the input list.


Testing in GHCi

ghci> :load Ex7_19
ghci> quickCheck propISort
+++ OK, passed 100 tests.

-}