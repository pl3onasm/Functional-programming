module Ex3_24 where

import Ex3_23

-----------------------------------------------------------
-- Exercise 3.24

-- | Returns the larger root of a quadratic equation
--   with given coefficients a, b and c
largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
  | a == 0 && b /= 0 = -c / b   -- linear case
  | nRoots == 0 = 0             -- no roots   
  | nRoots == 3 = 0             -- all numbers are roots
  | otherwise   = max root1 root2
    where
      disc = sqrt (b^2 - 4*a*c)
      root1 = (-b + disc) / (2*a)
      root2 = (-b - disc) / (2*a)
      nRoots = numberRoots a b c

-- | Returns the smaller root of a quadratic equation
--   with given coefficients a, b and c
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | a == 0 && b /= 0 = -c / b   -- linear case
  | nRoots == 0 = 0             -- no roots   
  | nRoots == 3 = 0             -- all numbers are roots
  | otherwise   = min root1 root2
    where
      disc = sqrt (b^2 - 4*a*c)
      root1 = (-b + disc) / (2*a)
      root2 = (-b - disc) / (2*a)
      nRoots = numberRoots a b c


-----------------------------------------------------------

{-

Note that we have to work with min and max because we do
not know the sign of a.

-}