module Ex3_20 where

-----------------------------------------------------------
-- Exercise 3.20

-- | Returns the average of three integers
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromIntegral (x + y + z) / 3.0

-- | Determines how many inputs are larger than their 
-- average value
howManyAboveAverage :: Integer -> Integer -> Integer 
                       -> Integer
howManyAboveAverage x y z = grAvg x + grAvg y + grAvg z
  where grAvg n = if fromIntegral n > avg then 1 else 0
        avg = averageThree x y z

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex3-20
ghci> averageThree 15 21 25
20.333334
ghci> howManyAboveAverage 15 21 25
2
ghci> howManyAboveAverage 5 5 5
0
ghci> howManyAboveAverage 1 2 3
1

-}