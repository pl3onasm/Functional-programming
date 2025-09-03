import Test.QuickCheck

type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.11

-- | Flips a picture in a horizontal mirror
flipH :: Picture -> Picture
flipH = reverse

-- | Stacks two pictures, one above the other
above :: Picture -> Picture -> Picture
above = (++)

-- | Property: stacking two pictures and then flipping them
-- in a horizontal mirror is the same as flipping each
-- picture in a horizontal mirror and then stacking them
-- but in reverse order.
prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH pic1 pic2 = 
        flipH (pic1 `above` pic2) 
    == (flipH pic2) `above` (flipH pic1)


-----------------------------------------------------------

{-

The orginal property was stated incorrectly as:

    flipH (pic1 `above` pic2) 
        == (flipH pic1) `above` (flipH pic2)

It did not reverse the order of the pictures on the right 
hand side of the equality.

Testing in GHCi

ghci> :l Ex6_11
ghci> quickCheck prop_AboveFlipH
+++ OK, passed 100 tests.

-}