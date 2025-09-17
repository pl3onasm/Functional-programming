module Chapter_07.Ex7_34 where

-----------------------------------------------------------
-- Exercise 7.34

-- | Replaces the first occurrence of a substring (old)
-- in a string with another substring (new)
subst :: String -> String -> String -> String
subst old new str = search str
  where 
  search [] = []
  search s@(c : cs)
    | startsWith old s = new ++ drop (length old) s
    | otherwise        = c : search cs

  startsWith [] _ = True
  startsWith _ [] = False
  startsWith (x : xs) (y : ys) = x == y && startsWith xs ys


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex7_35
ghci> subst "much  " "tall " "How much  is that?"
"How tall is that?"
ghci> subst "is" "was" "This is it"
"This was it"

-}

