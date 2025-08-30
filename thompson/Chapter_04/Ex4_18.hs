import Chapter_04.Ex4_17

-----------------------------------------------------------
-- Exercise 4.18

-- | Computes the factorial of a given natural number n
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = rangeProduct 1 n


-----------------------------------------------------------

{-

The function factorial is defined as a special case of
the function rangeProduct from Exercise 4.17, where the
first argument is 1 and the second argument is n. 
As an exceptional case, the factorial of 0 is defined to 
be 1.

Testing in GHCi

ghci> :set -i.. 
ghci> :l Ex4_18
ghci> factorial 5
120
ghci> factorial 0
1

-}