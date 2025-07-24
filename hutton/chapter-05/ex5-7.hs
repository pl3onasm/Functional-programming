import Test.QuickCheck
-----------------------------------------------------------
-- Exercise 5.7

-- generates the pairs (1,3),(1,4),(2,3),(2,4)
pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [1,2], y <- [3,4]]

-- does the same, but instead of using one comprehension
-- with two generators, this definition uses two 
-- comprehensions each having a single generator
pairs' :: [(Int, Int)]
pairs' = concat [[(x, y) | y <- [3,4]] | x <- [1,2]]

-----------------------------------------------------------

{- 

The first definition iterates over each x in [1,2] and 
for each x, iterates over every y in [3,4].

In the second definition, the inner comprehension

  [(x, y) | y <- [3,4]] 

produces a list of pairs for a fixed x. This value for x
is given by the outer comprehension which loops over x in 
[1,2], generating a list of lists.
This result is then flattened into a single list of pairs
by using concat.

We can actually check that the two definitions are indeed
equivalent by using QuickCheck as illustrated below.

-}

prop_pairsEq :: Bool
prop_pairsEq = pairs == pairs'

-- To test, in GHCi run:
-- quickCheck prop_pairsEq
-- 
-- Expected output:
-- +++ OK, passed 100 tests.


