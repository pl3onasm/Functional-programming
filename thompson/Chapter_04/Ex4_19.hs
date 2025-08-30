-----------------------------------------------------------
-- Exercise 4.19

-- | Defines multiplication as repeated addition for
-- natural numbers using primitive recursion.
mul :: Integer -> Integer -> Integer
mul m n
  | m == 0    = 0
  | otherwise = n + mul (m - 1) n 

-----------------------------------------------------------

{-

This definition uses the (+) operator to define 
multiplication applying primitive recursion. 

Base case: 
  multiplying 0 by any n yields 0.
Recursive case: 
  add n to the result of multiplying (m-1) by n.

The first argument is the number of times we add the 
second argument to itself, and merely acts as a counter.

-}