-----------------------------------------------------------
-- Exercise 5.13

type Radius = Float
type Height = Float
type Width  = Float

data Point = Point Float Float
  deriving (Eq, Show)

data NewShape
  = Circle Point Radius
  | Rectangle Point Height Width
  deriving (Eq, Ord, Show)

overlaps :: NewShape -> NewShape -> Bool
overlaps (Circle (Point x1 y1) r1) 
         (Circle (Point x2 y2) r2) 
  = (x1 - x2)^2 + (y1 - y2)^2 < (r1 + r2)^2

overlaps (Rectangle (Point x1 y1) h1 w1) 
         (Rectangle (Point x2 y2) h2 w2) 
  = not (x1 + w1/2 <= x2 - w2/2 || x2 + w2/2 <= x1 - w1/2 
      || y1 + h1/2 <= y2 - h2/2 || y2 + h2/2 <= y1 - h1/2)

overlaps _ _ = False  


-----------------------------------------------------------

{-

To determine if two shapes overlap, we implement the 
overlaps function that takes two NewShape values and 
returns a Bool indicating whether they overlap. 
The function currently handles only the cases where both 
shapes are circles or both are rectangles.

For circles, we calculate the distance between their 
centers and compare it to the sum of their radii. If the 
distance is less than the sum of the radii, the circles 
overlap.
For rectangles, we check if one rectangle is completely 
to the left, right, above, or below the other rectangle. 
If none of these conditions are true, the rectangles 
overlap.

Testing in GHCi

ghci> :l Ex5_12
ghci> c1 = Circle (Point 0 0) 5
ghci> c2 = Circle (Point 3 4) 5
ghci> c3 = Circle (Point 10 10) 2
ghci> c1 `overlaps` c2
True
ghci> c1 `overlaps` c3
False
ghci> r1 = Rectangle (Point 0 0) 4 6
ghci> r2 = Rectangle (Point 2 1) 4 6
ghci> r3 = Rectangle (Point 10 10) 2 2
ghci> r1 `overlaps` r2
True
ghci> r1 `overlaps` r3
False

-}