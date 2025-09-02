module Chapter_04.Ex4_02 where

-----------------------------------------------------------
-- Exercise 4.2

-- | Returns the middle number of three integers
middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber x y z
  | between y x z = x
  | between x y z = y
  | otherwise     = z
  where
    between x y z = x <= y && y <= z || z <= y && y <= x

-----------------------------------------------------------

{-

Note that we need to check both ascending and descending 
order in the between function. If we only checked ascending 
order, we would get wrong results in cases like:
middleNumber 1 0 -1 or milddleNumber 3 3 2, which would 
then return -1 and 2, respectively, instead of 0 and 3.

Testing in GHCi

ghci> :l Ex4_02
ghci> middleNumber 2 3 3
3
ghci> middleNumber 2 (-3) 5
2
ghci> middleNumber (-1) (-3) (-2)
-2
ghci> middleNumber (-1) (-3) 2
-1

-}