module Chapter_05.Ex5_07 where

-----------------------------------------------------------
-- Exercise 5.7

type Radius = Float
type Height = Float
type Width  = Float
type Side   = Float

data Shape = Circle Radius 
           | Rectangle Height Width
           | Triangle Side Side Side
  deriving (Eq,Ord,Show)


perimeter :: Shape -> Float
perimeter (Circle r)      = 2 * pi * r
perimeter (Rectangle h w) = 2 * (h + w)
perimeter (Triangle a b c) = a + b + c

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w
area (Triangle a b c) = 
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound _          = False


-----------------------------------------------------------

{-

Creating type synonyms for the basic types used in the data
declaration makes the code more readable and easier to 
understand with zero extra effort or runtime cost. It is
also preferred to using comments to indicate the meaning of 
the basic types, as comments can get out of date and add 
clutter to the code.

Testing in GHCi

ghci> :l Ex5_07
ghci> tri = Triangle 3 4 5
ghci> area tri
6.0
ghci> perimeter tri
12.0
ghci> isRound tri
False

-}