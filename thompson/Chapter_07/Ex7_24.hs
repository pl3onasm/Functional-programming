-----------------------------------------------------------
-- Exercise 7.24

-- | Original quicksort function 
qSort :: [Integer] -> [Integer]
qSort [] = []
qSort (x : xs)
  = qSort smaller ++ [x] ++ qSort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger  = [y | y <- xs, y > x]

-- | Modified qSort to sort in descending order
qSortDesc :: [Integer] -> [Integer]
qSortDesc [] = []
qSortDesc (x : xs)
  = qSortDesc larger ++ [x] ++ qSortDesc smaller
  where
    smaller = [y | y <- xs, y <= x]
    larger  = [y | y <- xs, y > x]

-- | Modified qSort to remove duplicates
qSortNoDup :: [Integer] -> [Integer]
qSortNoDup [] = []
qSortNoDup (x : xs)
  = qSortNoDup smaller ++ [x] ++ qSortNoDup larger
  where
    smaller = [y | y <- xs, y < x]
    larger  = [y | y <- xs, y > x]


-----------------------------------------------------------

{-

The first modification sorts the list in descending order 
by switching the order of the recursive calls, placing the
larger elements first.

The second modification removes duplicates by changing the
condition y <= x to y < x in the definition of 'smaller'.
This ensures that duplicate elements equal to x are not 
included again. The result is a sorted list of unique 
elements.

Testing in GHCi

ghci> :load Ex7_24
ghci> qSort [7,3,-1,6,2,-7,9,8,-8,0,1,5,3,7,0,-3]
[-8,-7,-3,-1,0,0,1,2,3,3,5,6,7,7,8,9]
ghci> qSortDesc [7,3,-1,6,2,-7,9,8,-8,0,1,5,3,7,0,-3]
[9,8,7,7,6,5,3,3,2,1,0,0,-1,-3,-7,-8]
ghci> qSortNoDup [7,3,-1,6,2,-7,9,8,-8,0,1,5,3,7,0,-3]
[-8,-7,-3,-1,0,1,2,3,5,6,7,8,9]

-}