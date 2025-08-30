module Chapter_04.Ex4_23 where
  
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 4.23

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n
  | n == 0 = f 0
  | n > 0  = sumFun f (n - 1) + f n

-- | Returns the number of regions defined by n lines
-- using sumFun
regions :: Integer -> Integer
regions n = 1 + sumFun f n
  where f x = x

-- | Original definition of regions for comparison
regions' :: Integer -> Integer
regions' n
  | n == 0 = 1
  | n > 0  = regions (n-1) + n

-- | Property to check regions and regions' are equivalent
prop_regions :: Integer -> Bool
prop_regions n
  | n < 0     = True   -- ignore negative numbers
  | otherwise = regions n == regions' n


-----------------------------------------------------------

{-

The function regions computes the number of regions defined 
by n lines. We redefined it in terms of sumFun. 
The key observation is that regions computes the sum of the
first n natural numbers, plus one. Setting f x = x,
we can use sumFun to compute the first sum and add one to
get the same result.

For example, regions 3 computes 1 + 1 + 2 + 3 = 7
And sumFun f 3 computes 1 + (f 0 + f 1 + f 2 + f 3)
  = 1 + (0 + 1 + 2 + 3) = 7


Testing with QuickCheck

ghci> :l Ex4_23
ghci> quickCheck prop_regions
+++ OK, passed 100 tests.

-}



