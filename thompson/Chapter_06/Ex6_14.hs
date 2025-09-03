import Test.QuickCheck
import Chapter_06.Ex6_08

type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.14

-- | Rotates a picture 180 degrees
rotate :: Picture -> Picture
rotate = flipH . flipV

-- | Alternative definition for a 180 degrees rotation
rotate' :: Picture -> Picture
rotate' = flipV . flipH

-- | Checks that a picture is non-empty
notEmpty :: Picture -> Bool
notEmpty pic = not (null pic) && any (not . null) pic

-- | Property: rotating a picture four times is the same as
-- the original picture.
prop_rotate90_four :: Picture -> Property
prop_rotate90_four pic =
  notEmpty pic ==>
    let p = padToRect pic
    in (rotate90 . rotate90 . rotate90 . rotate90) p == p

-- | Property: rotating a picture 90 degrees twice is the
-- same as rotating it 180 degrees.
prop_rotate90_twice :: Picture -> Property
prop_rotate90_twice pic =
  notEmpty pic ==>
    let p = padToRect pic
    in (rotate90 . rotate90) p == rotate p

-- | Property: rotating a picture 180 degrees using either
-- definition gives the same result.
prop_rotate :: Picture -> Property
prop_rotate pic = 
  notEmpty pic ==>
    let p = padToRect pic
    in rotate p == rotate' p

-- | Property: rotating a picture 180 degrees twice is the
-- same as the original picture.
prop_rotate_twice :: Picture -> Property
prop_rotate_twice pic =
  notEmpty pic ==>
    let p = padToRect pic
    in (rotate . rotate) p == p


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_14
ghci> quickCheck prop_rotate90_four
+++ OK, passed 100 tests, 19 discarded.
ghci> quickCheck prop_rotate90_twice
+++ OK, passed 100 tests, 16 discarded.
ghci> quickCheck prop_rotate
+++ OK, passed 100 tests, 26 discarded.
ghci> quickCheck prop_rotate_twice
+++ OK, passed 100 tests, 21 discarded.

-}