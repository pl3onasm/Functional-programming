module Chapter_04.Ex4_17 where

-----------------------------------------------------------
-- Exercise 4.17

-- | Computes the range product between two given natural 
-- numbers m and n as: m * (m+1) * ... * (n-1) * n
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n 
  | n == m = m
  | n > m  = n * rangeProduct m (n-1)
  | otherwise = 0


-----------------------------------------------------------

{-

The definition of rangeProduct is a modification of the
factorial function from the book. The only difference is
that the base case occurs when n equals m, in which case
the function returns m. 

Of course, this works only when n is greater than or equal
to m. Therefore, as the problem statement requires, when n
is smaller than m, the function returns 0.

Testing in GHCi

ghci> :l Ex4_17
ghci> rangeProduct 3 5
60
ghci> rangeProduct 5 5
5
ghci> rangeProduct 5 3
0

-}