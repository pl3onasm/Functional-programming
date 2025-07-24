-----------------------------------------------------------
-- Exercise 5.9

-- takes two lists of equal length, and returns their 
-- scalar product
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-----------------------------------------------------------

{- Example

  ghci> :l ex5-9.hs
  ghci> scalarproduct [1,2,3] [4,5,6]
  32

-}