module Chapter_06.Ex6_08 where

import Chapter_06.Ex6_07
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 6.8

-- | Flips a picture in a vertical mirror
flipV :: Picture -> Picture
flipV pic = [reverse line | line <- pic]

-- | Flips a picture in a horizontal mirror
flipH :: Picture -> Picture
flipH = reverse

-- | Rotates a picture 180 degrees
rotate :: Picture -> Picture
rotate = flipH . flipV

-- | Transposes a picture: each row becomes a column 
-- and vice versa
transpose :: Picture -> Picture
transpose pic
  | null (head pic) = []
  | otherwise       = [head line | line <- pic]
                    : transpose [tail line | line <- pic]

-- | Rotates a picture 90 degrees clockwise
rotate90 :: Picture -> Picture
rotate90 = transpose . flipV

-- | Alternative definition for a 90 degrees
-- clockwise rotation
rotate90' :: Picture -> Picture
rotate90' = flipH . transpose

-- | Pads a picture with '.' characters so that
-- it becomes rectangular
padToRect :: Picture -> Picture
padToRect [] = []
padToRect pic =
  [ln ++ replicate (maxLen - length ln) '.' | ln <- pic]
  where 
    maxLen = maximum [length line | line <- pic]

-- | Property: rotating a non-empty rectangular picture 90 
-- degrees clockwise using either definition gives the same
-- result
prop_rotate90 :: Picture -> Property
prop_rotate90 pic =
  not (null pic) && any (not . null) pic ==>
    let p = padToRect pic
    in rotate90 p == rotate90' p


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex6_08
ghci> pic = [".##.", ".#.#", ".###", "####"]
ghci> printPicture pic
.##.
.#.#
.###
####
ghci> p = transpose pic
ghci> printPicture p
...#
####
#.##
.###
ghci> printPicture (flipV p)
#...
####
##.#
###.

From the above we see that transpose first converts columns
into rows. That result is then flipped in a vertical mirror
to give a 90 degree clockwise rotation.

The same result can be achieved by first flipping the
original picture in a horizontal mirror and then
transposing it, as shown below:

ghci> p2 = flipH pic
ghci> printPicture p2
####
.###
.#.#
.##.
ghci> printPicture (transpose p2)
#...
####
##.#
###.

Because of the way pictures are represented as lists of
strings, this will only work for rectangular pictures,
i.e. where all the strings in the list have the same
length. To handle non-rectangular pictures, we can
first pad them with '.' characters to make them 
rectangular. 

Now we can test the property prop_rotate90 in GHCi to
check that both definitions of rotate90 give the same
result for randomly generated pictures:
ghci> quickCheck prop_rotate90
+++ OK, passed 100 tests; 18 discarded.

The discarded tests are those where the picture is empty
or where all the strings in the picture are empty. We only
want to consider rectangular pictures with at least some
visible content.

-}