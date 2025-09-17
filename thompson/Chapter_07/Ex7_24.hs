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
condition y <= x to y < x in the definition of 'smaller',
resulting in only unique elements being included in the 
'smaller' list. This ensures that duplicate elements equal 
to x are not included again. The result is a sorted list 
of unique elements.

-}