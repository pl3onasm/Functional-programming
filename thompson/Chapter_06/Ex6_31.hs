-----------------------------------------------------------
-- Exercise 6.31

type Picture = [[Char]]
type Position = (Int, Int)
type Image = (Picture, Position)

moveIm :: Image -> Int -> Int -> Image
moveIm (pic, (x, y)) dx dy = (pic, (x + dx, y + dy))


-----------------------------------------------------------