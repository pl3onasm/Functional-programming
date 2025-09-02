-----------------------------------------------------------
-- Exercise 5.11

type Radius = Float
type Height = Float
type Width  = Float
type Side   = Float

data Point = Point Float Float
  deriving (Eq, Ord, Show)

data Shape
  = Circle Point Radius
  | Rectangle Point Height Width
  | Triangle Point Side Side Side
  deriving (Eq, Ord, Show)

-----------------------------------------------------------

{-

To extend the Shape type to include a position represented 
by a center point, we define a new data type Point to
represent the coordinates of the center. The Shape data
type is then modified so that each shape constructor takes
a Point as its first argument, representing the center of
the shape.

-}