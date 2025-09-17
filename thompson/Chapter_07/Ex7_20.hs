import Prelude hiding (take, drop, splitAt)
import qualified Prelude (take, drop, splitAt)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.20

-- | Returns the first n elements of a list
take :: Int -> [a] -> [a]
take 0 _      = []
take _ []     = []
take n (x : xs) 
  | n > 0     = x : take (n-1) xs
  | otherwise = error "take: negative argument"

-- | Drops the first n elements from a list
drop :: Int -> [a] -> [a]
drop 0 xs     = xs 
drop _ []     = []
drop n (_ : xs) 
  | n > 0     = drop (n-1) xs
  | otherwise = error "drop: negative argument"

-- | Splits a list at a given position into a pair of lists
splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 xs  = ([], xs)
splitAt _ []  = ([], [])
splitAt n (x : xs)
  | n > 0     = let (ys, zs) = splitAt (n-1) xs
                in  (x : ys, zs)
  | otherwise = error "splitAt: negative argument"

-- | Alternative definition of splitAt using take and drop
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

-- | Property to test that both definitions of splitAt 
-- are equivalent
propSplit :: NonNegative Int -> [Int] -> Bool
propSplit (NonNegative n) xs =
  splitAt n xs == splitAt' n xs

-- | Property to test against Prelude's splitAt
propSplitAt :: NonNegative Int -> [Int] -> Bool
propSplitAt (NonNegative n) xs =
  splitAt n xs == Prelude.splitAt n xs

-- | Property to test take against Prelude's take
propTake :: NonNegative Int -> [Int] -> Bool
propTake (NonNegative n) xs =
  take n xs == Prelude.take n xs

-- | Property to test drop against Prelude's drop
propDrop :: NonNegative Int -> [Int] -> Bool
propDrop (NonNegative n) xs =
  drop n xs == Prelude.drop n xs


-----------------------------------------------------------

{-

The implementation of drop mirrors that of take, except 
that the base case returns the whole list when n is zero, 
and the recursive case discards the head of the list.

The implementation of splitAt uses pattern matching for the 
base cases, and recursion to build up the result. A let 
binding captures the result of the recursive call to 
splitAt, which is then used to construct the final result. 
A where clause would achieve the same effect, but the let 
binding better reflects the operational reading (reflecting
how recursion unfolds) of the definition:

  "to split the list at n, first split the tail at n-1, 
   then add the head to the first part of the result".

We also provide an alternative definition of splitAt using 
take and drop. This version is simpler, but less efficient, 
since take and drop each traverse the list, resulting in 
two passes instead of one.

In the properties, NonNegative Int is used to ensure that 
the input to take, drop, and splitAt is always a non-
negative integer. This type wrapper is provided by the 
QuickCheck library. It is necessary because our 
implementations throw errors for negative arguments, 
whereas the Prelude functions do not.

Testing in GHCi

ghci> :l Ex7_20
ghci> quickCheck propSplit
+++ OK, passed 100 tests.
ghci> quickCheck propSplitAt
+++ OK, passed 100 tests.
ghci> quickCheck propTake
+++ OK, passed 100 tests.
ghci> quickCheck propDrop
+++ OK, passed 100 tests.

-}
