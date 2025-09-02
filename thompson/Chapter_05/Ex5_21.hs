import Prelude hiding (elem)

-----------------------------------------------------------
-- Exercise 5.21

matches :: Integer -> [Integer] -> [Integer]
matches n xs = [n | x <- xs, x == n]

elem :: Integer -> [Integer] -> Bool
elem n xs = [] /= matches n xs


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex5_21
ghci> matches 3 [1,2,3,4,3,5,3]
[3,3,3]
ghci> matches 0 [1,2,3,4,3,5,3]
[]
ghci> elem 3 [1,2,3,4,3,5,3]
True
ghci> elem 6 [1,2,3,4,3,5,3]
False

-}