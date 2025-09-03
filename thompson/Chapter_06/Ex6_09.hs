import Chapter_06.Ex6_07
import Chapter_06.Ex6_08
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 6.9

-- | Rotates a picture 90 degrees counter-clockwise
rotateMin90 :: Picture -> Picture
rotateMin90 = flipV . transpose

-- | Alternative definition using flipH
rotateMin90' :: Picture -> Picture
rotateMin90' = transpose . flipH

-- | Alternative definition using three clockwise rotations
rotateMin90'' :: Picture -> Picture
rotateMin90'' = rotate90 . rotate90 . rotate90

-- | Property: rotating a non-empty picture 90 degrees
-- counter-clockwise using any of the three definitions
-- gives the same result
prop_rotateMin90 :: Picture -> Property
prop_rotateMin90 pic =
  not (null pic) && any (not . null) pic ==>
    let p = padToRect pic
    in rotateMin90 p == rotateMin90' p
       && rotateMin90 p == rotateMin90'' p


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_09
ghci> pic = [".##.", ".#.#", ".###", "####"]
ghci> printPicture pic
.##.
.#.#
.###
####
ghci> p = flipV pic
ghci> printPicture p
.##.
#.#.
###.
####
ghci> printPicture (transpose p)
.###
#.##
####
...#

From the above we see that the original picture is first
flipped in a vertical mirror, and then transposed to give
a 90 degree counter-clockwise rotation.

The same result can be achieved by first transposing the
original picture and then flipping it in a horizontal
mirror, or by applying three clockwise rotations:

ghci> p = transpose pic
ghci> printPicture p
...#
####
#.##
.###
ghci> printPicture (flipH p)
.###
#.##
####
...#

ghci> p = rotate90 pic
ghci> printPicture p
#...
####
##.#
###.
ghci> p2 = rotate90 p
ghci> printPicture p2
...#
####
#.##
.###
ghci> printPicture (rotate90 p2)
.###
#.##
####
...#

We now run the property prop_rotateMin90 in GHCi to check  
that all three definitions give the same result for 
randomly generated non-empty pictures:
ghci> quickCheck prop_rotateMin90
+++ OK, passed 100 tests; 12 discarded.

The discarded tests are those where the picture is empty
or where all the strings in the picture are empty. We only
want to consider rectangular pictures with at least some
visible content.

-}

