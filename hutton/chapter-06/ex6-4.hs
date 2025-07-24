-----------------------------------------------------------
-- Exercise 6.4

-- takes two non-negative integers and computes their gcd
-- using Euclid's algorithm
euclid :: Int -> Int -> Int
euclid a b
  | a == b    = a
  | a > b     = euclid (a - b) b
  | otherwise = euclid a (b - a) 

-----------------------------------------------------------

{- 
  Examples

  ghci> :l ex6-4.hs
  ghci> euclid 18 12
  6
  ghci> euclid 97 10
  1

-}