-----------------------------------------------------------
-- Exercise 4.21

-- | Returns the maximum of the values f 0, f 1, ..., f n
maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f n
  | n < 0     = error "maxFun: negative argument"
  | n == 0    = f 0
  | otherwise = max (f n) (maxFun f (n - 1))


-----------------------------------------------------------

{-

The function is defined by primitive recursion on n.
Base case: 
  when n is 0, the maximum is simply f 0.
Recursive case:
  take the maximum of f n and the maximum of the preceding
  values f 0, f 1, ..., f (n-1).

Testing in GHCi
ghci> :l Ex4_21
ghci> g x = x*x - 3*x + 2
ghci> maxFun g 3
2
ghci> maxFun g 4
6
ghci> maxFun g 5
12
ghci> f 0 = 3
ghci> f 1 = 17
ghci> f 2 = 44
ghci> f _ = 0
ghci> maxFun f 1
17
ghci> maxFun f 2
44
ghci> maxFun f 3
44

-}