-----------------------------------------------------------
-- Exercise 5.5

type Radius = Float
type Height = Float
type Width  = Float

data Shape = Circle Radius 
           | Rectangle Height Width
  deriving (Eq,Ord,Show)


perimeter :: Shape -> Float
perimeter (Circle r)      = 2 * pi * r
perimeter (Rectangle h w) = 2 * (h + w)


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex5_05
ghci> circ = Circle 3
ghci> rect = Rectangle 3 4
ghci> perimeter circ
18.849556
ghci> perimeter rect
14.0


-}