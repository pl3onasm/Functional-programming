-----------------------------------------------------------
-- Exercise 5.10

type Radius = Float
type Height = Float
type Width  = Float
type Side   = Float

data Shape = Circle Radius 
           | Rectangle Height Width
           | Triangle Side Side Side
  deriving (Ord, Show)

instance Eq Shape where

  (Circle r1) == (Circle r2)
    | r1 < 0 && r2 < 0   = True     -- both invalid
    | r1 >= 0 && r2 >= 0 = r1 == r2 -- both valid
    | otherwise = False    -- one valid, one invalid

  (Rectangle h1 w1) == (Rectangle h2 w2)
    | invalid h1 w1 && invalid h2 w2 = True
    | not (invalid h1 w1) && not (invalid h2 w2)
        = h1 == h2 && w1 == w2
    | otherwise = False
    where
      invalid h w = h < 0 || w < 0

  (Triangle a1 b1 c1) == (Triangle a2 b2 c2)
    | invalid a1 b1 c1 && invalid a2 b2 c2 = True
    | not (invalid a1 b1 c1) && not (invalid a2 b2 c2)
        = a1 == a2 && b1 == b2 && c1 == c2
    | otherwise = False
    where
      invalid a b c = a < 0 || b < 0 || c < 0
    
  -- Different shapes are never equal
  _ == _ = False


-----------------------------------------------------------

{-

The purpose of this exercise is to define a custom equality
operation for the Shape type, instead of using the default
derived by Haskell. The custom equality should consider
shapes with invalid dimensions (negative values) as equal
to each other, while valid shapes are only equal if their
dimensions match exactly.

The implementation defines the (==) operator for the Shape
type by pattern matching on the different constructors:
Circle, Rectangle, and Triangle. As it is defined now, the
the positions of the dimensions are respected, so that 
Triangle 3 4 5 is not equal to Triangle 4 3 5. If we wanted
to ignore the order of the sides, we could sort them before
comparing or use a more complex comparison function.

Testing in GHCi

ghci> :load Ex5_10
ghci> Circle 3 == Circle 3
True
ghci> Circle (-1) == Circle (-2)
True
ghci> Circle (-1) == Circle 2
False
ghci> Rectangle 2 3 == Rectangle 2 3
True
ghci> Rectangle (-1) 3 == Rectangle (-2) 4
True
ghci> Triangle 3 5 4 == Triangle 5 3 4
False



-}