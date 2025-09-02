module Chapter_05.Ex5_02 where

import Chapter_04.Ex4_02 (middleNumber)
import Chapter_03.Ex3_13 (maxThree)
import Chapter_03.Ex3_14 (minThree)

-----------------------------------------------------------
-- Exercise 5.2

-- | Puts the numbers of the triple in ascending order
orderTriple :: Integer -> Integer -> Integer -> 
               (Integer, Integer, Integer)
orderTriple x y z = (a, b, c)
  where
    a = minThree x y z
    c = maxThree x y z
    b = middleNumber x y z


-----------------------------------------------------------