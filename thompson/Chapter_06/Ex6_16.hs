import Test.QuickCheck
import Chapter_06.Ex6_08 (padToRect)

type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.16

-- | Places two pictures beside each other
beside :: Picture -> Picture -> Picture
beside picL picR
    = [lineL ++ lineR | (lineL,lineR) <- zip picL picR]

-- | Stacks two pictures, one above the other
above :: Picture -> Picture -> Picture
above = (++)

prop_AboveBeside1 :: Picture -> Picture -> Property
prop_AboveBeside1 n s =
  (n `beside` s) `above` (n `beside` s) 
    === (n `above` n) `beside` (s `above` s)

-- | Gets the height of a picture
height :: Picture -> Int
height = length

-- | Property: modified version of prop_AboveBeside1 where
-- the two pictures are required to have the same height
prop_AboveBeside2 :: Picture -> Picture -> Property
prop_AboveBeside2 n s =
  height n == height s ==>
    (n `beside` s) `above` (n `beside` s) 
      === (n `above` n) `beside` (s `above` s)

-- | Pads two pictures to the same height by adding
-- '.' lines at the bottom of the shorter one
padToSameHeight :: Picture -> Picture -> (Picture, Picture)
padToSameHeight [] pic = (pic, pic)
padToSameHeight pic [] = (pic, pic)
padToSameHeight pic1 pic2 = (pad pic1, pad pic2)
  where
    maxH = max (height pic1) (height pic2)
    pad pic = pic ++ replicate (maxH - height pic) 
                     (replicate (length (head pic)) '.')

-- | Property: modified version of prop_AboveBeside1 where
-- the two pictures are first padded to the same height
prop_AboveBeside3 :: Picture -> Picture -> Property
prop_AboveBeside3 n s =
  let (n', s') = padToSameHeight n s
  in (n' `beside` s') `above` (n' `beside` s') 
       === (n' `above` n') `beside` (s' `above` s')


-----------------------------------------------------------

{-

If we try to test the property prop_AboveBeside1 in GHCi:
ghci> :l Ex6_16
ghci> quickCheck prop_AboveBeside1
*** Failed! Falsified (after 5 tests and 7 shrinks):
[""]
["","a"]
["",""] /= ["","a"]

We see that it fails. The problem is that the pictures are
generated randomly and so they may have different heights.
To fix this, we first define a function to get the height
of a picture, and then we add a precondition to the 
property to ensure that the two pictures have the same
height. We can then test the modified property:

ghci> quickCheck prop_AboveBeside2
*** Gave up! Passed only 44 tests; 1000 discarded tests.

The message indicates that only 44 out of 1000 randomly 
generated pictures satisfied the precondition. This is 
because chances of two randomly generated pictures having
the same height are very low. This is why we got so many
discarded tests and why QuickCheck gave up: it did 
manage to find 100 valid cases in the default 1000 tests.

If we want to avoid discarded tests, we can define a 
function to pad two pictures to the same height and then
use that function in the property. Testing this modified
property does not give any discarded tests:

ghci> quickCheck prop_AboveBeside3
+++ OK, passed 100 tests.

-}