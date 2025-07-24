-----------------------------------------------------------
-- Exercise 4.3

-- returns the tail of the input list, but in a safe way
-- i.e. returns [] if the input is the empty list
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- same, but using guarded equations
safetail' xs
  | null xs = []
  | otherwise = tail xs

-- same, but using pattern matching
safetail'' [] = []
safetail'' xs = tail xs

-----------------------------------------------------------

{- Examples:

  ghci> :l ex4-3.hs
  ghci> safetail ['h','e','l','l','o','w','o','r','l','d']
  "elloworld"
  ghci> safetail [1,2,3,4,5,6]
  [2,3,4,5,6]
  
-}