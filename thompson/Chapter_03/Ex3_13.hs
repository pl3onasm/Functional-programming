module Chapter_03.Ex3_13 where

import Prelude hiding (max)

-----------------------------------------------------------
-- Exercise 3.13

-- | computes the maximum of two integers
max :: Integer -> Integer -> Integer
max x y
  | x >= y    = x
  | otherwise = y

-- | computes the maximum of three integers
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
  | x >= y && x >= z = x
  | y >= z           = y
  | otherwise        = z

-----------------------------------------------------------

{-

1. Calculation for max (3-2) (3*8)

  max (3-2) (3*8)
=   { evaluating - and * }
  max 1 24
=   { applying max, check guard: 1 >= 24 is False, 
                    so use otherwise }
  24
  
2. Calculattion for maxThree (4+5) (2*6) (100 ‘div‘ 7)

  maxThree (4+5) (2*6) (100 `div` 7)
=   { evaluating +, *, and div }
  maxThree 9 12 14
=   { applying maxThree, check first guard:
      9 >= 12 && 9 >= 14 is False }
  maxThree 9 12 14
=   { applying maxThree, check second guard:
      12 >= 14 is False, so use otherwise }
  14

-}