import Prelude hiding (max)

-----------------------------------------------------------
-- Exercise 4.1

-- | Computes the maximum of two integers
max :: Integer -> Integer -> Integer
max x y
  | x >= y    = x
  | otherwise = y

-- | Computes the maximum of three integers
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
  | x >= y && x >= z = x
  | y >= z           = y
  | otherwise        = z

-- | Returns the maximum of four integers, modelled on 
-- the definition of maxThree
maxFour :: Integer -> Integer -> Integer -> Integer 
        -> Integer
maxFour w x y z 
  | w >= x && w >= y && w >= z = w
  | x >= y && x >= z           = x
  | y >= z                     = y
  | otherwise                  = z

-- | Returns the maximum of four integers, using max
maxFour' :: Integer -> Integer -> Integer -> Integer 
         -> Integer
maxFour' w x y z = max (max w x) (max y z)

-- | Returns the maximum of four integers, using maxThree
-- and max
maxFour'' :: Integer -> Integer -> Integer -> Integer 
          -> Integer
maxFour'' w x y z = max w (maxThree x y z)

-- | Returns the maximum of four integers. This is another 
-- variant using maxThree and max
maxFour''' :: Integer -> Integer -> Integer -> Integer 
           -> Integer
maxFour''' w x y z = maxThree (max w x) (max y z) z

-----------------------------------------------------------