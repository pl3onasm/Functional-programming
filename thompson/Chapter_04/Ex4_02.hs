-----------------------------------------------------------
-- Exercise 4.2

-- | Returns the middle number of three integers
middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber x y z
  | between y x z = x
  | between x y z = y
  | otherwise     = z
  where
    between x y z = x <= y && y <= z

-----------------------------------------------------------

{-

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