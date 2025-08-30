-----------------------------------------------------------
-- Exercise 4.20

-- | Returns the integer square root of the input integer
intSqrt :: Integer -> Integer
intSqrt n = sqt (n `div` 2)
  where 
    sqt 0 = 0
    sqt x
      | x * x <= n = x
      | otherwise  = sqt(x - 1)

-----------------------------------------------------------

{-

The function intSqrt starts searching for the integer 
square root from n `div` 2 downwards. This is in fact just 
a linear search.
Of course, this is not very efficient, but it follows the 
pattern of primitive recursion. A more efficient version 
would use binary search.


Testing in GHCi

ghci> :l Ex4_20
ghci> intSqrt 16
4
ghci> intSqrt 15
3
ghci> intSqrt 1
1
ghci> intSqrt 0
0


-}