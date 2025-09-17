import Prelude hiding (reverse, unzip)
import qualified Prelude (reverse, unzip)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.11

-- | Reverses a list
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- | Unzips a list of pairs into a pair of lists
unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y) : zs) = let (xs, ys) = unzip zs 
                      in (x : xs, y : ys)

-- | Property to test if our reverse is equivalent 
-- to Prelude.reverse
propReverse :: [Int] -> Bool
propReverse xs = reverse xs == Prelude.reverse xs

-- | Property to test if our unzip is equivalent
-- to Prelude.unzip
propUnzip :: [(Int, Char)] -> Bool
propUnzip xs = unzip xs == Prelude.unzip xs


-----------------------------------------------------------

{-

Reverse works by recursively reversing the tail of the list
and then appending the head element to the end of the 
reversed tail. The base case is when the list is empty,
in which case we return an empty list.

The base case for unzip turns an empty list of pairs into a
pair of empty lists. The recursive case splits the head 
pair (x, y) and then prepends them to the lists obtained
from unzipping the tail zs.

Testing in GHCi

ghci> :load Ex7_11
ghci> quickCheck propReverse
*** Failed! Falsified (after 4 tests and 4 shrinks):
+++ OK, passed 100 tests.
ghci> quickCheck propUnzip
+++ OK, passed 100 tests.
ghci>

-}