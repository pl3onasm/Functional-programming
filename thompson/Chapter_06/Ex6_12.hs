import Test.QuickCheck

type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.12

-- | Flips a picture in a horizontal mirror
flipH :: Picture -> Picture
flipH = reverse

-- | Flips a picture in a vertical mirror
flipV :: Picture -> Picture
flipV pic
  = [reverse line | line <- pic]

-- | Places two pictures beside each other
beside :: Picture -> Picture -> Picture
beside picL picR
    = [lineL ++ lineR | (lineL,lineR) <- zip picL picR]

-- | Property: placing two pictures beside each other and 
-- then flipping them in a horizontal mirror is the same as
-- flipping each picture in a horizontal mirror and then
-- placing them beside each other.
prop_BesideFlipH :: Picture -> Picture -> Bool
prop_BesideFlipH pic1 pic2 = 
                    flipH (pic1 `beside` pic2) 
                == (flipH pic1) `beside` (flipH pic2)

-- | Property: placing two pictures beside each other and 
-- then flipping them in a vertical mirror is the same as
-- flipping each picture in a vertical mirror and then
-- placing them beside each other but in reverse order.
prop_BesideFlipV :: Picture -> Picture -> Bool
prop_BesideFlipV pic1 pic2 = 
                    flipV (pic1 `beside` pic2) 
                == (flipV pic2) `beside` (flipV pic1)


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_12
ghci> quickCheck prop_BesideFlipH
+++ OK, passed 100 tests.
ghci> quickCheck prop_BesideFlipV
+++ OK, passed 100 tests.

-}