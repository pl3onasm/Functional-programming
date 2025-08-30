import Test.QuickCheck
import Chapter_03.Ex3_20

-----------------------------------------------------------
-- Exercise 3.21

-- | Property to test averageThree
prop_avg3 :: Integer -> Integer -> Integer -> Bool
prop_avg3 x y z = 
  3 * averageThree x y z == fromIntegral (x + y + z)

-- | Property to test howManyAboveAverage: result must
-- be between 0 and 2
prop_howManyAboveAvg :: Integer -> Integer -> Integer 
                        -> Bool
prop_howManyAboveAvg x y z = n >= 0 && n <= 2
  where n = howManyAboveAverage x y z


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex3-21
ghci> quickCheck prop_avg3
+++ OK, passed 100 tests.
ghci> quickCheck prop_howManyAboveAvg
+++ OK, passed 100 tests.

-}