-----------------------------------------------------------
-- Exercise 9.1

-- Define a functions cons' that behaves as : but is 
-- defined in terms of ++
cons' :: a -> [a] -> [a]
cons' x xs = [x] ++ xs

-----------------------------------------------------------
-- Exercise 9.3

-- Takes an integer n and a value v, and creates a list 
-- containing n times v
repl :: Int -> a -> [a]
repl n v = aux n []
  where
    aux 0 xs = xs
    aux n xs = aux (n - 1) (v : xs)

-- Note that we are building the list via an accumulator.
-- This is more efficient, especially for large n, as
-- it avoids deep recursion and stack overflow.
-- The non-tail-recursive version would be:

repl' :: Int -> a -> [a]
repl' 0 _ = []
repl' n v = v : repl' (n - 1) v

-----------------------------------------------------------
-- Exercise 9.4

-- Takes a number n and returns a list of the first n
-- prime numbers
takePrimes :: Int -> [Int]
takePrimes n = aux n 2 []
  where
    aux 0 p xs = reverse xs
    aux n p xs
      | isPrime p = aux (n - 1) (p + 1) (p : xs)
      | otherwise = aux n (p + 1) xs

-- Checks whether a number p is prime
isPrime :: Int -> Bool
isPrime p 
  | p <= 1 = False
  | otherwise = null [x | x <- [2..up], p `mod` x == 0]
      where up = floor $ sqrt $ fromIntegral p

-- This could be done more efficiently by using a sieve
-- and infinite lists, but that has not been covered yet

-----------------------------------------------------------
-- Exercise 9.5

-- Takes a triple and produces a list with all permutations
-- of the elements in the triple
permuteTriple :: (Eq a) => (a, a, a) -> [(a, a, a)]
permuteTriple (x, y, z) =
  [(x, y, z), (x, z, y), (y, x, z), 
   (y, z, x), (z, x, y), (z, y, x)]

-----------------------------------------------------------
-- Exercise 9.7

-- Verifies if a list is sorted in ascending order
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs)
  | x <= y = isSorted (y : xs)
  | otherwise = False

-----------------------------------------------------------
-- Exercise 9.8

-- Removes duplicates from a list preserving the first 
-- occurrence of each element (maintaining order)
delDups :: (Eq a) => [a] -> [a]
delDups xs = delDups' xs []
  where
    delDups' [] _ = []
    delDups' (x : xs) seen
      | x `elem` seen = delDups' xs seen
      | otherwise = x : delDups' xs (x : seen)

-----------------------------------------------------------
-- Exercise 9.9

-- Same as delDups but assuming the input list is sorted
delDupsSorted :: (Eq a) => [a] -> [a]
delDupsSorted [] = []
delDupsSorted [x] = [x]
delDupsSorted (x : y : xs)
  | x == y = delDupsSorted (y : xs)
  | otherwise = x : delDupsSorted (y : xs)

-----------------------------------------------------------
-- Exercise 9.10

-- Takes an integer n and a list, and returns the prefix
-- of the list of length at most n
takePrefix :: Int -> [a] -> [a]
takePrefix _ [] = []
takePrefix 0 _ = []
takePrefix n (x : xs) = x : takePrefix (n - 1) xs

-----------------------------------------------------------
-- Exercise 9.11

-- Takes a value v and a sorted list, and returns the 
-- sorted list with v inserted at the correct position
insert :: (Ord a) => a -> [a] -> [a]
insert v [] = [v]
insert v (x : xs)
  | v > x = x : insert v xs
  | otherwise = v : x : xs

-----------------------------------------------------------
-- Exercise 9.12

-- Sorts a given list using the function insert from 9.11
-- This is an efficient, tail-recursive implementation
inSort :: (Ord a) => [a] -> [a]
inSort xs = srt xs []
  where
    srt [] sorted = sorted
    srt (x : xs) sorted = srt xs (insert x sorted)

-- A less efficient non-tail-recursive implementation:
insSort :: (Ord a) => [a] -> [a]
insSort [] = []
insSort (x : xs) = insert x (insSort xs)

{- 
  In the second definition, the recursive call 
  insSort xs must complete before insert x can be 
  applied. This creates a chain of deferred operations 
  (stack frames), which consumes more stack memory,
  and can lead to a stack overflow for large input lists.

  The first version avoids this by using an accumulator,
  which allows the function to be tail-recursive and more 
  space-efficient.
-}

-----------------------------------------------------------
-- Exercise 9.13

-- Takes two lists, and verifies if the first is a sublist
-- of the second
isSubl :: (Eq a) => [a] -> [a] -> Bool
isSubl [] _ = True
isSubl _ [] = False
isSubl (x : xs) (y : ys)
  | x == y = isSubl xs ys
  | otherwise = isSubl (x : xs) ys

-----------------------------------------------------------
-- Exercise 9.14

-- Takes two sorted lists (ascending order) and merges them 
-- into one sorted list
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-----------------------------------------------------------
-- Exercise 9.15

-- Splits a given list of length n into two sublists of 
-- length at most ⌈n/2⌉ 
split' :: [a] -> ([a], [a])
split' xs = splt xs [] (length xs `div` 2)
  where
    splt [] ys _ = (ys, [])
    splt xs ys 0 = (reverse ys, xs)
    splt (x : xs) ys n = splt xs (x : ys) (n - 1)


-- alternatively, we can use the functions take and drop
split'' :: [a] -> ([a], [a])
split'' xs = (take n xs, drop n xs)
  where 
    n = length xs `div` 2

-- or we can use splitAt:
split''' :: [a] -> ([a], [a])
split''' xs = splitAt (length xs `div` 2) xs
 
-----------------------------------------------------------
-- Exercise 9.16

{- Same as the previous exercise, but now we may only
   traverse the list once, so we cannot use the length
   function or reverse, as both require traversing the
   list.
-}

-- A more efficient implementation of split that
-- traverses the list only once
split :: [a] -> ([a], [a])
split xs = splt xs xs
  where
    splt (y : ys) (_ : _ : zs) = 
      let (as, bs) = splt ys zs in (y : as, bs)
    splt ys _ = ([], ys) 

{- This implementation basicallly traverses the entire 
   list only once but does it at two speeds: one element 
   at a time (the slow pointer) and two elements at a time
   (the fast pointer). 

   When the fast pointer reaches the end (base case), 
   the slow pointer is at the middle of the list. 
   At that point, we split the list. The remainder from 
   the slow pointer becomes the second half of the split,
   while the first half is built recursively as we 
   backtrack and prepend the elements to the
   accumulator (the first argument of the splt function).
   
   This is why the base case returns an empty list as the 
   first component of the pair: the first half is 
   constructed implicitly as the call stack unwinds.

   Also observe that if we can no longer take two elements 
   at a time, the function naturally falls into the base 
   case, which is why we do not need to check for the 
   length of the list in the beginning, and also why we can
   put an underscore in the second argument of the splt 
   function: it can be a singleton or an empty list, it 
   does not matter.
-}

-----------------------------------------------------------
-- Exercise 9.17

-- Implementation of the merge sort algorithm using the
-- functions defined in the previous exercises
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
  let (left, right) = split xs
  in merge (mergeSort left) (mergeSort right)

-----------------------------------------------------------
-- Exercise 9.18

-- Left-rotates a list by one position
rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft (x : xs) = xs ++ [x]

-----------------------------------------------------------
-- Exercise 9.19

-- Implementation of zip
zip' :: [a] -> [b] -> [(a, b)]
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys
zip' _ _ = []

-----------------------------------------------------------
-- Exercise 9.20

-- Removes all pairs with a given key from an association
-- list
delAll :: (Eq a) => a -> [(a, b)] -> [(a, b)]
delAll _ [] = []
delAll k ((k', v) : ps) 
  | k == k' = delAll k ps
  | otherwise = (k', v) : delAll k ps

-----------------------------------------------------------