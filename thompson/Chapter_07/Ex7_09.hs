module Chapter_07.Ex7_09 where

import Chapter_07.Ex7_08 (elemNum)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.9

-- | Returns a list of the unique elements in a list
-- of integers
unique :: [Integer] -> [Integer]
unique xs = uniq xs
  where
    uniq [] = []
    uniq (y : ys)
      | elemNum y xs == 1 = y : uniq ys
      | otherwise         = uniq ys

-- A different implementation using a list comprehension
unique2 :: [Integer] -> [Integer]
unique2 xs = [x | x <- xs, elemNum x xs == 1]

-- | Property to test that the first two implementations
-- of unique are equivalent
propUnique :: [Integer] -> Bool
propUnique xs = unique xs == unique2 xs

-- | Property to test that the last two implementations
-- of unique are equivalent
propUnique2 :: [Integer] -> Bool
propUnique2 xs = unique2 xs == unique3 xs

-- | A third implementation of unique that does not
-- use elemNum
unique3 :: [Integer] -> [Integer]
unique3 [] = []
unique3 (x : xs)
  | x `elem` xs = unique3 (del x xs)
  | otherwise   = x : unique3  xs
  where
    del _ [] = []
    del y (z : zs)
      | y == z    = del y zs
      | otherwise = z : del y zs


-----------------------------------------------------------

{-

In the first implementation, we have to define a local 
helper function uniq that does the actual work. This is 
because we need a reference to the original list xs in 
order to count the occurrences of each element in the 
original list. The function uniq follows the pattern of
primitive recursion over lists. 

Of course, using elemNum all the time is inefficient,
since it traverses the list each time it is called.
This is also true of the list comprehension version.

Alternatively, we can define a helper function that 
removes all occurrences of an element from a list. This
can be used to ensure that each element is only checked
once. This is what the third implementation does.

Even the third implementation is not very efficient, since
it uses elem to check for membership, which traverses the
list and when it finds a match, del traverses the list 
again to remove the element and all its occurrences. More 
effient would be to combine the membership test and 
deletion into a single pass.

Testing in GHCi

ghci> :set -i..
ghci> :l Ex7_09
ghci> unique [4,2,1,3,2,3]
[4,1]
ghci> unique' [4,2,1,3,2,3]
[4,1]
ghci> quickCheck propUnique
+++ OK, passed 100 tests.
ghci> quickCheck propUnique2
+++ OK, passed 100 tests.


-}