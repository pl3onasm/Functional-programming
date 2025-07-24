-----------------------------------------------------------
-- Exercise 5.6

-- takes an integer n and returns its list of factors
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- returns the list of all perfect numbers up to an input
-- limit n
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x) - x]

-----------------------------------------------------------

{- Example

  ghci> :l ex5-6.hs
  ghci> perfects 500
  [6,28,496]

-}