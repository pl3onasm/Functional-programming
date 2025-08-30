module Chapter_04.Ex4_03 where

import Chapter_03.Ex3_11 (threeEqual, threeDifferent)

-----------------------------------------------------------
-- Exercise 4.3

-- | Determines how many of the input values are equal
howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual x y z
  | threeEqual x y z     = 3
  | threeDifferent x y z = 0
  | otherwise            = 2


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i.. 
ghci> :l Ex4_03
ghci> howManyEqual 2 3 4
0
ghci> howManyEqual 2 3 2
2
ghci> howManyEqual 2 2 2
3

-}