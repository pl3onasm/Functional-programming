import Prelude hiding (and, or)
import qualified Prelude (and, or)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.6

-- | Computes the conjunction of a list of Bool values.
and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

-- | Computes the disjunction of a list of Bool values.
or :: [Bool] -> Bool 
or [] = False
or (x : xs) = x || or xs

-- | Property to test that our and function
-- is equivalent to Prelude's and function.
propAnd :: [Bool] -> Bool
propAnd xs = and xs == Prelude.and xs

-- | Property to test that our or function
-- is equivalent to Prelude's or function.
propOr :: [Bool] -> Bool
propOr xs = or xs == Prelude.or xs


-----------------------------------------------------------

{-

The definitions follow the pattern of primitive recursion 
over lists. The base case for and returns True, which is
the identity for conjunction, and the base case for or
returns False, which is the identity for disjunction, 
since we have for any Bool b:

  True  && b = b
  False || b = b


Testing in GHCi

ghci> :l Ex7_06
ghci> quickCheck propAnd
+++ OK, passed 100 tests.
ghci> quickCheck propOr
+++ OK, passed 100 tests.

-}