import Prelude hiding (all, any, takeWhile, dropWhile)

-----------------------------------------------------------
-- Exercise 7.2

-- decides if all element of a list satisfy a predicate
all :: (a -> Bool) -> [a] -> Bool
all p = foldr (&&) True . map p

-- decides if any element of a list satisfies a predicate
any :: (a -> Bool) -> [a] -> Bool 
any p = foldr (||) False . map p

-- selects elements from a list while they satisfy a 
-- predicate
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs) 
  | p x       = x : takeWhile p xs
  | otherwise = []

-- removes element from a list while they satisfy a 
-- predicate
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p xs      = dropWhile p xs
  | otherwise = x : xs

-----------------------------------------------------------