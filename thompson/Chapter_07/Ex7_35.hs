import Chapter_07.Ex7_34 (subst)
import Test.QuickCheck
import Data.List (isInfixOf)

-----------------------------------------------------------
-- Exercise 7.35

-- | Property: If the old substring does not occur in the
-- string, the result is the original string as long as
-- the old substring is not empty
propNoOld :: String -> String -> String -> Property
propNoOld old new st =
  not (old `isInfixOf` st) && not (null old) ==>
    subst old new st == st

-- | Property: If the old and new substrings are the same,
-- the result is the original string
propSameOldNew :: String -> String -> Bool
propSameOldNew sub str = subst sub sub str == str

-- | Property: If the old substring occurs in the string,
-- and the new substring does not, then substituting old 
-- with new and then new with old returns the original 
-- string
propRoundTrip :: String -> String -> String -> Property
propRoundTrip old new st =
  not (new `isInfixOf` st) ==>
    subst new old (subst old new st) == st


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex7_35
ghci> quickCheck propNoOld
+++ OK, passed 100 tests; 14 discarded.
ghci> quickCheck propSameOldNew
+++ OK, passed 100 tests.
ghci> quickCheck propRoundTrip
+++ OK, passed 100 tests; 12 discarded.

The discarded tests are those where the preconditions are
not met, which is expected.

-}