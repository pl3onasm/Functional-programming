-----------------------------------------------------------
-- Exercise 6.2

-- takes an integer n and returns the sum from n down to 0
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-----------------------------------------------------------

{- 
  Note the strong similarity to the factorial function.
  The base case here is 0, because the identity of
  addition is 0, not 1. The recursive case is also
  similar, but instead of multiplying n by the factorial
  of (n - 1), we add n to the sum of the numbers from 
  (n - 1) down to 0.
-}