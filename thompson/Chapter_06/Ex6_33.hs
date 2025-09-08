import Chapter_06.Ex6_08 (rotate90, flipH, flipV, rotate)

-----------------------------------------------------------
-- Exercise 6.33, naive view: the picture's position does 
-- not change, only the picture itself

type Picture = [[Char]]
type Position = (Int, Int)
type Image = (Picture, Position)

-- | Flips an image in a vertical mirror
rotateIm :: Image -> Image
rotateIm (pic, pos) = (rotate pic, pos)

-- | Rotates an image 90 degrees CW
rotate90Im :: Image -> Image
rotate90Im (pic, pos) = (rotate90 pic, pos)

-- | Flips an image in a vertical mirror
flipImV :: Image -> Image
flipImV (pic, pos) = (flipV pic, pos)

-- | Flips an image in a horizontal mirror
flipImH :: Image -> Image
flipImH (pic, pos) = (flipH pic, pos)


-----------------------------------------------------------