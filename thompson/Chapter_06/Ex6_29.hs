-----------------------------------------------------------
-- Exercise 6.29

type Picture = [[Char]]
type Position = (Int, Int)
type Image = (Picture, Position)

makeIm :: Picture -> Position -> Image
makeIm pic pos = (pic, pos)


-----------------------------------------------------------