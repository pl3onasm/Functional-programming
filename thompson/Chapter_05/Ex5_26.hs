-----------------------------------------------------------
-- Exercise 5.26

-- | Computes the n-th Fibonacci number in O(n)
fib :: Integer -> Integer
fib n = fst (fibPair n)
  where 
    fibPair 0 = (0, 1)
    fibPair n = let (a, b) = fibPair (n-1) 
                in (b, a + b)

-- | Generates a table of Fibonacci numbers from 0 to n.
fibTable :: Integer -> String
fibTable n = "n\tfib n\n" ++ concat 
  [show i ++ "\t    " ++ show(fib i) ++ "\n" | i <- [0..n]]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex5_26
ghci> putStr (fibTable 10)
n       fib n
0           0
1           1
2           1
3           2
4           3
5           5
6           8
7           13
8           21
9           34
10          55



-}