-----------------------------------------------------------
-- Exercise 4.1

-- takes a list and returns a tuple containing two lists:
-- the first and second half of the input list. Input list
-- can have even or odd length.
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-----------------------------------------------------------

{- Examples:

  ghci> :l ex4-1.hs
  ghci> halve [1,2,3,4,5,6,7]
  ([1,2,3],[4,5,6,7])
  ghci> halve ['h','e','l','l','o','w','o','r','l','d']
  ("hello","world")

-}