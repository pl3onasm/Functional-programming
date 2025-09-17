module Chapter_07.Ex7_16 where

-----------------------------------------------------------
-- Exercise 7.16

-- | Insertion sort (descending order)
iSort1 :: [Integer] -> [Integer]
iSort1 []       = []
iSort1 (x : xs) = ins1 x (iSort1 xs) 

-- | Inserts an element into a sorted list
ins1 :: Integer -> [Integer] -> [Integer]
ins1 x [] = [x]
ins1 x (y : ys)
  | x >= y    = x : (y : ys)
  | otherwise = y : ins1 x ys

-- | Insertion sort (removing duplicates to 
-- produce a strictly increasing list)
iSort2 :: [Integer] -> [Integer]
iSort2 []       = []
iSort2 (x : xs) = ins2 x (iSort2 xs) 

-- | Inserts an element into a sorted list 
-- without duplicates
ins2 :: Integer -> [Integer] -> [Integer]
ins2 x [] = [x]
ins2 x (y : ys)
  | x < y     = x : (y : ys)
  | x == y    = y : ys
  | otherwise = y : ins2 x ys


-----------------------------------------------------------

{-

The first function, iSort1, sorts a list of integers in 
descending order. This is achieved by modifying the 
comparison operator in the ins1 function from x <= y
to x >= y.

The second function, iSort2, sorts a list of integers
while removing duplicates. This is done by changing the
comparison operator in the ins2 function from x <= y
to x < y and adding an additional condition to handle the
case when x == y, in which case the duplicate is skipped.

Testing in GHCi

ghci> :load Ex7_16
ghci> iSort1 [3, 1, 4, 1, 5, 9, 2, 6, 5]
[9,6,5,5,4,3,2,1,1]
ghci> iSort2 [3, 1, 4, 1, 5, 9, 2, 6, 5]
[1,2,3,4,5,6,9]


-}