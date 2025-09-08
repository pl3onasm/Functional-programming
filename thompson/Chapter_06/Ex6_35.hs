module Chapter_06.Ex6_35 where

-----------------------------------------------------------
-- Exercise 6.35

type Picture = [[Char]]
type Position = (Int, Int)  -- reference point (x,y)
type Image = (Picture, Position)

-- | Checks if an image is empty
emptyIm :: Image -> Bool
emptyIm (pic, _) = null pic || all null pic 

-- | Pads an image with white space
pad :: Int -> Int -> Int -> Int -> Image -> Image
pad left right bottom top im@(pic, (x, y)) 
  | emptyIm im = im
  | any (< 0) [left, right, bottom, top] = 
      error "pad: negative padding"
  | otherwise = (newPic, (x - left, y - bottom))
  where
    w = length (head pic)
    newWidth  = w + left + right
    padLine ln = replicate left '.' ++ ln ++ replicate right '.'
    midLines  = [padLine ln | ln <- pic]
    topLines    = replicate top (replicate newWidth '.')
    bottomLines = replicate bottom (replicate newWidth '.')
    newPic = topLines ++ midLines ++ bottomLines


-----------------------------------------------------------

{- 

Padding adds extra white space around a picture. The four
parameters specify the number of columns (left, right) and
rows (bottom, top) to be added.

The effect on the picture’s reference point (its bottom-
left corner) is:

  newX = oldX - left
  newY = oldY - bottom

That is, left padding shifts the reference point left by 
the amount of padding, and bottom padding shifts it down. 
Right and top padding do not affect the reference point.

Example:

  Suppose an image has reference point (10,5). If we apply
  padImage 2 3 1 4, then

    newX = 10 - 2 = 8
    newY = 5 - 1 = 4

  so the new reference point is (8,4).

The padded picture has new dimensions:

  newWidth  = oldWidth  + left + right
  newHeight = oldHeight + bottom + top

so in this example, if the original picture was 6×4, the
padded one will be 11×9.

-}