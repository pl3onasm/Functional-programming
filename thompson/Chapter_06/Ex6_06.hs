import Chapter_06.Ex6_05

type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.6

superimpose :: Picture -> Picture -> Picture
superimpose pic1 pic2 = [superimposeLine line1 line2 
                        | (line1, line2) <- zip pic1 pic2]


-----------------------------------------------------------

