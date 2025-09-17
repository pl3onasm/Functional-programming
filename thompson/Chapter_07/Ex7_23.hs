import Prelude hiding (zip3)
import qualified Prelude (zip3)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.23

-- | Zips three lists into a list of triples
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 [] _ _ = []
zip3 _ [] _ = []
zip3 _ _ [] = []
zip3 (x : xs) (y : ys) (z : zs)
  = (x, y, z) : zip3 xs ys zs

-- | Alternative definition using zip
zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' xs ys zs = 
  [(x, y, z) | (x, (y, z)) <- zip xs (zip ys zs)]

-- | Property: zip3 and Prelude.zip3 are equivalent
propZip3 :: (Eq a, Eq b, Eq c) 
            => [a] -> [b] -> [c] -> Bool
propZip3 xs ys zs = 
  zip3 xs ys zs == Prelude.zip3 xs ys zs

-- | Property: zip3' and Prelude.zip3 are equivalent
propZip3' :: (Eq a, Eq b, Eq c) 
              => [a] -> [b] -> [c] -> Bool
propZip3' xs ys zs = 
  zip3' xs ys zs == Prelude.zip3 xs ys zs


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :load Ex7_23
ghci> quickCheck propZip3
+++ OK, passed 100 tests.
ghci> quickCheck propZip3'
+++ OK, passed 100 tests. 

What are the advantages and disadvantages of the two 
definitions of zip3?

The first definition uses direct recursion, which is 
straightforward and easy to understand. It clearly shows
the truncation behavior at the base cases. 
This implementation also has good performance, as it
processes the lists in a single pass and does not build
intermediate structures. Lastly, it is easy to extend
to zip more than three lists by following the same pattern.

The second definition uses the built-in zip function and a
list comprehension. This makes it more concise and 
leverages existing functionality. However, it is less
efficient because it creates an intermediate list of pairs
using zip ys zs, which adds overhead. Additionally, the
truncation behavior is less explicit, as it relies on the
behavior of zip. Extending this approach to more than three
lists would also become increasingly complex and less 
readable.

Overall, both are correct and pass the tests, but the first
definition is arguably more transparent and efficient.

-}