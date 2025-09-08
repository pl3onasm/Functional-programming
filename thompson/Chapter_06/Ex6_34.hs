module Chapter_06.Ex6_34 where

import Chapter_06.Ex6_08 (rotate90, rotate, flipH, flipV)

-----------------------------------------------------------
-- Exercise 6.34, geometrical view: the picture's position 
-- changes according to the transformation

type Picture = [[Char]]
type Position = (Int, Int)  -- reference point (x,y)
type Image = (Picture, Position)


-- | Flips an image in a vertical mirror 
flipImV :: Image -> Image
flipImV ([], (x,y)) = ([], (x,y))
flipImV (pic, (x, y)) = (flipV pic, (x + w, y))
  where w = length (head pic) - 1

-- | Flips an image in a horizontal mirror 
flipImH :: Image -> Image
flipImH ([], (x,y)) = ([], (x,y))
flipImH (pic, (x, y)) = (flipH pic, (x, y - h))
  where h = length pic - 1

-- | Rotates an image 180 degrees 
rotateIm :: Image -> Image
rotateIm ([], (x,y)) = ([], (x,y))
rotateIm (pic, (x, y)) = (rotate pic, (x + w, y - h))
  where 
    w = length (head pic) - 1
    h = length pic - 1

-- | Rotates an image 90 degrees CW
rotate90Im :: Image -> Image
rotate90Im ([], (x,y)) = ([], (x,y))
rotate90Im (pic, (x, y)) = (rotate90 pic, (x + w, y))
  where 
    w = length (head pic) - 1


-----------------------------------------------------------

{-

It is crucial to keep in mind that the reference point for 
the image's position is the picture's bottom-left corner.
See Figure 6.9 in the book.

That means that when we flip the image vertically, the
reference point moves to the right by the width of the
picture minus one (because the reference point is already
at the left edge of the picture). Similarly, when we flip
the image horizontally, the reference point moves down
by the height of the picture minus one.

Since rotation by 180 degrees is defined as a composition
of a vertical and a horizontal flip, the reference point
moves right by the width minus one and down by the height
minus one. Finally, when we rotate the image 90 degrees
clockwise, the reference point moves right by the width
minus one (the new width is the old height, but the
reference point moves according to the old width).

Obviously, if the image is empty (the picture is empty),
then the reference point does not change. The image stays
put in the same position.

-}