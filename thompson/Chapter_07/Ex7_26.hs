import Data.List (isInfixOf, isSubsequenceOf)
import Chapter_07.Ex7_25 (isSublist, isSubseq)
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.26

-- | Property: isSubseq xs and isInfixOf xs are equivalent
propSubseq :: String -> String -> Bool
propSubseq xs ys = isSubseq xs ys == isInfixOf xs ys

-- | Property: isSublist xs and isSubsequenceOf xs are
-- equivalent
propSublist :: String -> String -> Bool
propSublist xs ys = 
  isSublist xs ys == isSubsequenceOf xs ys


-----------------------------------------------------------

{-

The functions isInfixOf and isSubsequenceOf are defined in
the module Data.List. These functions actually do the same
thing as isSublist and isSubseq respectively. So we can 
test our functions by checking that they give the same
results as the corresponding functions from Data.List.


Testing in GHCi

ghci> :load Ex7_25.hs
ghci> quickCheck propSubseq
+++ OK, passed 100 tests.
ghci> quickCheck propSublist
+++ OK, passed 100 tests.

-}