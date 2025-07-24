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
    
-}