-----------------------------------------------------------
-- Exercise 9.2

-- removes the first occurrence of a value from a list
del1st :: Eq a => a -> [a] -> [a]
del1st _ [] = []
del1st z (x : xs) 
  | z == x    = xs
  | otherwise = x : del1st z xs

-- decides if the first list is chosen (subselection)
-- from the second list
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) ys 
  | x `elem` ys = isChoice xs (del1st x ys)
  | otherwise   = False 

-----------------------------------------------------------

{- Examples

  ghci> :l ex9-2.hs
  ghci> isChoice [1,2] [2,3,3,1,4,1,5,1]
  True
  ghci> isChoice [1,1,1] [2,3,3,1,4,1,5,1]
  True
  ghci> isChoice [3,3,4,4] [2,3,3,1,4,1,5,1]
  False

-}