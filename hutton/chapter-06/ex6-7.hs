-----------------------------------------------------------
-- Exercise 6.7

-- takes two sorted lists and merges them into a single 
-- sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y    = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-----------------------------------------------------------

{- Example: evaluation of merge [1,3,3,5] [1,2,2,6,8]
  
    merge [1,3,3,5] [1,2,2,6,8]
  =     { applying merge }
    1 : merge [3,3,5] [1,2,2,6,8]
  =     { applying merge }
    1 : 1 : merge [3,3,5] [2,2,6,8]
  =     { applying merge }
    1 : 1 : 2 : merge [3,3,5] [2,6,8]
  =     { applying merge }
    1 : 1 : 2 : 2 : merge [3,3,5] [6,8]
  =     { applying merge }
    1 : 1 : 2 : 2 : 3 : merge [3,5] [6,8]
  =     { applying merge }
    1 : 1 : 2 : 2 : 3 : 3 : merge [5] [6,8]
  =     { applying merge }
    1 : 1 : 2 : 2 : 3 : 3 : 5 : merge [] [6,8]
  =     { applying merge }
    1 : 1 : 2 : 2 : 3 : 3 : 5 : [6,8]
  =     { applying (:) }
    [1,1,2,2,3,3,5,6,8]

-}