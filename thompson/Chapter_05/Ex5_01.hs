module Chapter_05.Ex5_01 where

-----------------------------------------------------------
-- Exercise 5.1

-- | Returns the maximum of two integers and the number 
-- of times it occurs
maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs x y
  | x > y     = (x, 1)
  | x < y     = (y, 1)
  | otherwise = (x, 2)

-- | Returns the maximum of three integers and the number
-- of times it occurs
maxThreeOccurs :: Integer -> Integer -> Integer -> 
                  (Integer, Integer)
maxThreeOccurs x y z 
  | z > m     = (z, 1)
  | z < m     = (m, c)
  | otherwise = (m, c + 1)
    where 
      (m, c) = maxOccurs x y


-----------------------------------------------------------