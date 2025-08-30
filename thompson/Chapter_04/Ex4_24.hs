import Chapter_04.Ex4_23 (sumFun)

-----------------------------------------------------------
-- Exercise 4.24

-- | Computes the maximum number of pieces in 2D by making
-- n straight cuts
regions2D :: Integer -> Integer
regions2D n = 1 + n*(n + 1) `div` 2

-- | Computes the maximum number of pieces in 3D by making
-- n planar cuts
regions3D :: Integer -> Integer
regions3D n
  | n < 0     = error "regions3D: negative argument"
  | n == 0    = 1
  | otherwise = regions3D (n-1) + regions2D (n-1)

-- | Alternative definition using sumFun
regions3D' :: Integer -> Integer
regions3D' n = 1 + sumFun regions2D (n-1)


-----------------------------------------------------------

{-

The function regions2D is the same as the one defined in 
the book for computing the maximum number of regions formed 
by n lines on a 2D surface. We use its closed-form 
expression here, which comes from the formula for the sum 
of the first n natural numbers.

The function regions3D computes the maximum number of 
regions formed by n planes in 3D. The key insight is that 
to obtain the maximum number of 3D regions when adding the 
n-th plane, we must position this new plane so that it
intersects all previous planes and the intersection lines 
on its surface produce the maximum number of 2D regions. 
These 2D regions correspond to the new 3D regions created 
by adding the n-th plane.

The recursive definition of regions3D reflects this idea.
Each new plane adds as many new 3D regions as there are 2D
regions formed on its surface by the intersection lines.
The base case is when there are no planes, in which case 
the space consists of a single region.

The alternative definition regions3D' uses the sumFun 
function from Exercise 4.23 to sum up the contributions
of each plane added, starting from 0 planes up to n-1
planes. Here, regions2D is passed as the function to sum, 
giving the same result as regions3D.


Testing in GHCi

ghci> :set -i..
ghci> :l Ex4_24
ghci> regions2D 3
8
ghci> regions3D 5
26
ghci> regions3D' 5
26
ghci> regions3D 10
176

-}