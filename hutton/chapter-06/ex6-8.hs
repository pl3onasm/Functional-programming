-----------------------------------------------------------
-- Exercise 6.8

-- takes two sorted lists and merges them into a single 
-- sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y    = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- splits a list into two halves whose lengths differ by
-- at most one
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-- takes a list and sorts it in ascending order
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = halve xs

-----------------------------------------------------------

{- Example: evaluation of msort [5,1,0,5,3]

    msort [5,1,0,5,3]
  =     { applying msort }
    merge (msort [5,1]) (msort [0,5,3])
  =     { applying msort }
    merge (merge (msort [5]) (msort [1])) (msort [0,5,3])
  =     { applying msort }
    merge (merge [5] [1]) (merge (msort [0]) (msort [5,3]))
  =     { applying merge and msort }
    merge [1,5] (merge [0] (msort [5,3]))
  =     { applying msort }
    merge [1,5] (merge [0] (merge (msort [5]) (msort [3])))
  =     { applying msort }
    merge [1,5] (merge [0] (merge [5] [3]))
  =     { applying merge }
    merge [1,5] (merge [0] [3,5])
  =     { applying merge }
    merge [1,5] [0,3,5]
  =     { applying merge }
    [0,1,3,5,5]
    

Note:

The sort is in O(n log n) time complexity, where n is
the length of the input list. That is because each level
of recursion takes O(n) time to merge the two sorted 
halves, and there are O(log n) levels of recursion due to 
the halving of the list at each level.

At each level of recursion, the list is split into two
halves using take and drop. This split takes O(n) time, but
it traverses the list twice. It is possible to improve on 
this constant factor of two by using two empty lists as
accumulators, and traversing the list only once to evenly
distribute the elements into the two accumulators. It
would look like this:

  split :: [a] -> ([a], [a])
  split xs = splt xs ([], [])
    where
      splt []  (as, bs) = (as, bs)         -- even split
      splt [x] (as, bs) = ((x : as), bs)   -- odd split
      splt (x : y : zs) (as, bs) = splt zs (x : as, y : bs)

Mind that this function splits the list in a very 
particular way: one half contains all elements at even
indices, and the other half contains all elements at odd
indices, and both halves are in reverse order. However,
this does not matter for merge sort, since both halves
are sorted again in the recursive calls to mergeSort.
This optimization reduces the constant factor from roughly
two to one, thus making the split noticeably faster for
very large lists.

-}