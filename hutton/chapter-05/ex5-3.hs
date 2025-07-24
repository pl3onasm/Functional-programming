-----------------------------------------------------------
-- Exercise 5.3

-- takes two integers m and n, and returns a coordinate
-- grid of size m x n
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(i, j) | i <- [0..m], j <- [0..n]]

-- returns a coordinate square of input size n, excluding
-- the diagonal from (0,0) to (n,n)
square :: Int -> [(Int, Int)]
square n = [(i, j) | (i, j) <- grid n n, i /= j]

-----------------------------------------------------------

{- Example:

  ghci> :l ex5-3.hs
  ghci> square 6

  [(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
  (1,0),(1,2),(1,3),(1,4),(1,5),(1,6),
  (2,0),(2,1),(2,3),(2,4),(2,5),(2,6),
  (3,0),(3,1),(3,2),(3,4),(3,5),(3,6),
  (4,0),(4,1),(4,2),(4,3),(4,5),(4,6),
  (5,0),(5,1),(5,2),(5,3),(5,4),(5,6),
  (6,0),(6,1),(6,2),(6,3),(6,4),(6,5)]

-}