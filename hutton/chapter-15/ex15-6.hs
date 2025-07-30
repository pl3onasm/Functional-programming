-----------------------------------------------------------
-- Exercise 15.6

-- Computes the square root of a non-negative Double 
-- using Newton's method.
sqroot :: Double -> Double
sqroot x = head $ dropWhile notClose $ iterate next 1.0
  where
    next y = (y + x / y) / 2
    epsilon = 0.0001
    notClose y = abs (y * y - x) > epsilon

-----------------------------------------------------------

{-
  The function generates an infinite list of approximations 
  starting from 1.0, where each next approximation is 
  calculated by averaging the current approximation y 
  and x / y.

  The iteration continues until the square of the current 
  approximation is within a small distance ε (0.0001) of x, 
  at which point the approximation is returned.

  Examples:

    ghci> :l ex15-6.hs

    ghci> sqroot 9
    3.000000001396984

    ghci> sqroot 2
    1.4142156862745097
-}
