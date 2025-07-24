import Prelude hiding (and, concat, replicate, (!!), elem)

-----------------------------------------------------------
-- Exercise 6.6

-- takes a list and decides if all logical values in a list
-- are true
and :: [Bool] -> Bool
and [] = True 
and (x : xs) 
  | x         = and xs
  | otherwise = False

-- takes a list of lists, and flattens it, making it
-- one-dimensional
concat :: [[a]] -> [a]
concat [] = []
concat ([] : xss) = concat xss 
concat ((x : xs) : xss) = x : concat (xs : xss)

-- alternatively, we could use the (++) operator
-- however, this is less efficient, due to the cost of 
-- repeated use of (++)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

-- takes an integer n and some element, and produces
-- a list of n copies of that element
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

-- takes an index n and a list, and returns the element
-- at that index in the list (0-based indexing)
(!!) :: [a] -> Int -> a
[] !! _ = error "index out of bounds"
(x : xs) !! 0 = x
(x : xs) !! n
  | n < 0     = error "index out of bounds"
  | otherwise = xs !! (n - 1)

-- takes an element and a list, and decides if the element
-- is in the list
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys)
  | x == y    = True
  | otherwise = elem x ys

-----------------------------------------------------------

{- 
  Examples

  ghci> :l ex6-6.hs
  ghci> and [True, True, True]
  True
  ghci> and [True, False, True]
  False
  ghci> concat ["hello", " ", "world", "!"]
  "hello world!"
  ghci> concat' [[1,2], [3,4], [5]]
  [1,2,3,4,5]
  ghci> replicate 3 'a'
  "aaa"
  ghci> [1,2,3] !! 0
  1
  ghci> [1,2,3] !! 2
  3
  ghci> elem 'a' "banana"
  True
  ghci> elem 'x' "banana"
  False
  
-}