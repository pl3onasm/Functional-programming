import Chapter_05.Ex5_07

-----------------------------------------------------------
-- Exercise 5.8

isRegular :: Shape -> Bool
isRegular (Circle _)       = True
isRegular (Rectangle h w)  = h == w
isRegular (Triangle a b c) = a == b && b == c


-----------------------------------------------------------