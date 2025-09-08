module Chapter_06.Ex6_32 where
import Chapter_06.Ex6_07 (printPicture)

-----------------------------------------------------------
-- Exercise 6.32

type Picture = [[Char]]
type Position = (Int, Int)
type Image = (Picture, Position)

printIm :: Image -> IO ()
printIm (pic, _) = printPicture pic


-----------------------------------------------------------