-----------------------------------------------------------
-- Exercise 4.22

-- | Determines if one or more of the first n values of the
-- function f is zero
anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f n
  | n < 0     = error "anyZero: negative argument"
  | n == 0    = f 0 == 0
  | otherwise = (f n == 0) || anyZero f (n - 1)


-----------------------------------------------------------

{-

The function returns True as soon as it finds a value f k
that is zero, for some k in the range 0..n. If no such 
value is found, it returns False.
The function is defined by primitive recursion on n.

Base case: 
  when n is 0, the function checks if f 0 is zero.
Recursive case:
  it checks if f n is zero, if so, it returns True,
  otherwise it recurses to check the preceding values
  f 0, f 1, ..., f (n-1).

Testing in GHCi

ghci> :l Ex4_22
ghci> g x = x - 3
ghci> anyZero g 2
False
ghci> anyZero g 5
True

-}