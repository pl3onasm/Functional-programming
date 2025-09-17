module Chapter_07.Ex7_08 where
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.8

-- | Counts the number of occurrences of an integer
-- in a list of integers.
elemNum :: Integer -> [Integer] -> Integer
elemNum _ [] = 0
elemNum n (x : xs)
  | n == x    = 1 + elemNum n xs
  | otherwise = elemNum n xs

-- A different implementation using a list comprehension
elemNum' :: Integer -> [Integer] -> Integer
elemNum' n xs = sum [1 | x <- xs, x == n]

-- | Property to test that both implementations
-- of elemNum are equivalent.
propElemNum :: Integer -> [Integer] -> Bool
propElemNum n xs = elemNum n xs == elemNum' n xs


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex7_08
ghci> elemNum 5 [2,3,5,5,4,7,6,6,5,4,7,5]
4
ghci> elemNum 5 []
0
ghci> elemNum' 5 [2,3,5,5,4,7,6,6,5,4,7,5]
4
ghci> elemNum' 5 []
0
ghci> quickCheck propElemNum
+++ OK, passed 100 tests.


-}