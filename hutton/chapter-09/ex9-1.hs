-----------------------------------------------------------
-- Exercise 9.1

-- generates all subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where yss = subs xs

-- interleaves an element into a list at all possible 
-- positions
interl :: a -> [a] -> [[a]]
interl x [] = [[x]]
interl x (y : ys) = (x : y : ys) : map (y :) (interl x ys)

-- generates all possible permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interl x) (perms xs))

-- generates all permutations of all subsequences,
-- i.e. all ordered selections (including empty and full)
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- does the same as choices, but uses a list comprehension
-- instead of composition, concat, and map
choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys] 

-----------------------------------------------------------

{- 
  To test that choices' works correctly, we can compare it
  with choices. Since the order of generated lists does 
  not matter, we use sort from Data.List to compare 
  the results as multisets.

    ghci> :l ex9-1.hs
    ghci> import Data.List (sort)
    ghci> sort (choices [1..4]) == sort (choices' [1..4])
    True

  To test the equivalence more thoroughly, we can use
  QuickCheck. We constrain the input length to avoid
  performance issues due to combinatorial explosion.

    ghci> import Test.QuickCheck
    ghci> :{
    ghci| prop_equiv xs = 
    ghci|   length xs <= 8 ==>
    ghci|   sort (map sort (choices xs)) ==
    ghci|   sort (map sort (choices' xs))
    ghci| :}
    ghci> quickCheck prop_equiv
    +++ OK, passed 100 tests; 529 discarded.

  This result means that QuickCheck successfully tested 
  the property for 100 random lists (each of length up 
  to 8), and discarded 529 tests where the input list 
  was too long for our constraint.

  In chapter 16, we will explore formal reasoning 
  techniques for proving function equivalence. 
-}