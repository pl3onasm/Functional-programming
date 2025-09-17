import Prelude hiding (product)
import qualified Prelude (product)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.5

-- | Computes the product of a list of integers.
product :: [Int] -> Int
product [] = 1
product (x : xs) = x * product xs

-- | Property to test that our product function
-- is equivalent to Prelude's product function.
propProduct :: [Int] -> Bool
propProduct xs = product xs == Prelude.product xs


-----------------------------------------------------------

{-

This definition mirrors the definition of sum. The only 
difference is that the base case returns 1, the 
multiplicative identity, instead of 0, the additive 
identity.

Testing in GHCi 

ghci> :l Ex7_05
ghci> quickCheck prop_product
+++ OK, passed 100 tests.


-}