module Chapter_03.Ex3_14 where

import Prelude hiding (min)

-----------------------------------------------------------
-- Exercise 3.14

-- | computes the minimum of two integers
min :: Integer -> Integer -> Integer
min x y
  | x <= y    = x
  | otherwise = y

-- | computes the minimum of three integers
minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z
  | x <= y && x <= z = x
  | y <= z           = y
  | otherwise        = z

-----------------------------------------------------------
