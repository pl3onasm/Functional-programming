import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 3.10

-- | checks if four integers are all equal
fourEqual :: Integer -> Integer -> Integer -> Integer 
             -> Bool
fourEqual w x y z = (w == x) && (x == y) && (y == z)

-- | book's definition of threeEqual
threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual x y z = (x == y) && (y == z)

-- | alternative defintion of fourEqual using threeEqual
fourEqual' :: Integer -> Integer -> Integer -> Integer 
              -> Bool
fourEqual' w x y z = threeEqual w x y && (y == z)

-- | comparing the two definitions by testing
prop_4Eq :: Integer -> Integer -> Integer -> Integer 
            -> Bool
prop_4Eq w x y z = fourEqual w x y z == fourEqual' w x y z

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :load Ex3-11
ghci> quickCheck prop_4Eq
+++ OK, passed 100 tests.

-}