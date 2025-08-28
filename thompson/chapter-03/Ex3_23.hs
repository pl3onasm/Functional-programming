module Ex3_23 where

import Ex3_22

-----------------------------------------------------------
-- Exercise 3.23

-- | Returns the number of roots of a quadratic equation
--   with given coefficients a, b and c
numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c
  | a /= 0    = numberNDroots a b c
  | b /= 0    = 1
  | c /= 0    = 0
  | otherwise = 3


-----------------------------------------------------------