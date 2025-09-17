import Chapter_07.Ex7_09 (unique, unique')
import Chapter_07.Ex7_08 (elemNum)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.10

-- | Property linking elemNum and unique.
-- For any integer n and list xs: n is in the list of 
-- unique elements of xs iff n occurs exactly once in xs.
propElemNumUnique :: Integer -> [Integer] -> Bool
propElemNumUnique n xs = 
  (n `elem` unique xs) == (elemNum n xs == 1)


-----------------------------------------------------------

{-

Testing in GHCi
ghci> :set -i..
ghci> :l Ex7_10
ghci> quickCheck propElemNumUnique
+++ OK, passed 100 tests.

-}