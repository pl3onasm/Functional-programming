-----------------------------------------------------------
-- Exercise 5.12

type Radius = Float
type Height = Float
type Width  = Float

data Point = Point Float Float
  deriving (Eq, Show)

data NewShape
  = Circle Point Radius
  | Rectangle Point Height Width
  deriving (Eq, Ord, Show)

move :: Float -> Float -> NewShape -> NewShape
move dx dy (Circle (Point x y) r) = 
  Circle (Point (x + dx) (y + dy)) r
move dx dy (Rectangle (Point x y) h w) = 
  Rectangle (Point (x + dx) (y + dy)) h w


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex5_11
ghci> c = Circle (Point 3 4) 5
ghci> r = Rectangle (Point (-2) 1) 4 6
ghci> move 1 2 c
Circle (Point 4.0 6.0) 5.0
ghci> move 2 (-1) r
Rectangle (Point 0.0 0.0) 4.0 6.0

-}