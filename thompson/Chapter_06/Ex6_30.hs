-----------------------------------------------------------
-- Exercise 6.30

type Picture = [[Char]]
type Position = (Int, Int)
type Image = (Picture, Position)

changePosition :: Image -> Position -> Image
changePosition (pic, _) newPos = (pic, newPos)


-----------------------------------------------------------