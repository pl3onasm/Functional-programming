-----------------------------------------------------------
-- Exercise 4.8

-- | This function calculates the area of a triangle 
-- given the lengths of its three sides
triArea :: Float -> Float -> Float -> Float
triArea a b c
  | possible = sqrt(s*(s - a)*(s - b)*(s - c))
  | otherwise = 0
  where
    s = (a + b + c) / 2
    possible = a + b > c && a + c > b && b + c > a


-----------------------------------------------------------

{-

The condition possible checks whether the three sides can 
form a triangle using the triangular inequality. 

If no triangle can be formed, the function returns 0. 
Otherwise, it calculates the area using Heron's formula,
where s is the semi-perimeter of the triangle.

-}