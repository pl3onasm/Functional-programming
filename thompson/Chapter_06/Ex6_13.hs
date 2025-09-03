import Test.QuickCheck

type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.13

-- | Places two pictures beside each other
beside :: Picture -> Picture -> Picture
beside picL picR
    = [lineL ++ lineR | (lineL,lineR) <- zip picL picR]

-- | Stacks two pictures, one above the other
above :: Picture -> Picture -> Picture
above = (++)

-- | Property: placing a picture beside itself and then
-- stacking the result with itself is the same as
-- stacking the picture with itself and then placing
-- the result beside itself.
prop_BesideAbove :: Picture -> Bool
prop_BesideAbove pic =
    (pic `beside` pic) `above` (pic `beside` pic)
    == (pic `above` pic) `beside` (pic `above` pic)


-----------------------------------------------------------

{-

This is essentially a distributive law.

Testing in GHCi

ghci> :l Ex6_13
ghci> quickCheck prop_BesideAbove
+++ OK, passed 100 tests.

-}